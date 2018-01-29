(******************************************************************************)
(* Copyright (C) 2018 Evaluations and Language Resources Distribution         *)
(* Agency (ELDA) S.A.S (Paris, FRANCE), all rights reserved.                  *)
(* contact http://www.elda.org/ -- mailto:info@elda.org                       *)
(*                                                                            *)
(* This file is part of the elrc_to_odp ELDA ELRC-Share to Open Data Portal   *)
(* export tool.                                                               *)
(*                                                                            *)
(* elrc_to_odp is free software: you can redistribute it and/or modify it     *)
(* under the terms of the GNU General Public License as published by the      *)
(* Free Software Foundation, either version 3 of the License, or (at your     *)
(* option) any later version.                                                 *)
(*                                                                            *)
(* elrc_to_odp is distributed in the hope that it will be useful, but         *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of                 *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *)
(* General Public License for more details.                                   *)
(*                                                                            *)
(* You should have received a copy of the GNU General Public License          *)
(* along with elrc_to_odp, in the LICENSE file. If not, see                   *)
(* <http://www.gnu.org/licenses/>.                                            *)
(******************************************************************************)

(** This tool maps ELRC Share metadata attributes into Open Data Portal metadata
 *  attributes. Also, XML elements are mapped to JSON representations. *)

open Core
open Core_extended
open Re2.Std

open Elrc_common

module Api : sig
  val post_dataset: ?debug: bool -> config: Sexplib.Sexp.t -> dprivate: bool ->
                    update_type: [`Create | `Update] -> upload_data: bool ->
                    string -> string -> string -> unit
end = struct

  type license_t = {
    license_url: string;
    license_id: string;
    common_fields: license_common_t;
  }

  type dataset_t = {
    contact_name: string;
    contact_email: string;
    contact_telephone: string;
    (* lowercase filename without _md.xml *)
    name: string;
    metadata_language: string;
    rtype: string;
    license: license_t;
    status: string list;
    resources: (rdescription_t * resource_t) list;
    owner_org: string;
    (* always true or false; no need to mutate *)
    dprivate: bool;
    (* Always [{name: "Language Resources for the development of language 
                       technologies including Machine Translation"}]*)
    groups: group_t list;
    mutable url: string;
    mutable modified_date: Date.t;
    (* authority codes only *)
    mutable language: string list;
    common_fields: dataset_common_t;
  }

  (** Map ISO 639-1 to authority codes.*)
  let map_language_codes ~config language_file =
    Xml.parse_file language_file
    |> Xml.children
    |> List.map ~f: (fun language_record ->
        let language = {iso_639_1_code = unspecified;
                        authority_code = unspecified} in
        begin
          match language_record with
          | Xml.Element ("record", _, data) -> List.iter data ~f: (
              function
              | Xml.Element ("iso-639-1", _, Xml.PCData lang :: _) ->
                language.iso_639_1_code <- lang
              | Xml.Element ("authority-code", _, Xml.PCData lang :: _) ->
                language.authority_code <- lang
              | _ -> ())
          | _ -> ()
        end;
        language
      )
    |> List.filter ~f: (fun entry -> entry.iso_639_1_code <> unspecified)
    |> List.map ~f: (fun datum ->
        (datum.iso_639_1_code,
         string_of_sexp_path ~path: ".language_authority_seed_url" config ^
         datum.authority_code))

  let get_resource_from_elrc_file ~config ?(dprivate=true) ?languages fname =
    let id = generate_data_id fname in
    let data_fname = id ^ ".html" in
    let resource_data = {
      description = Data;
      rformat = unspecified;
      url = string_of_sexp_path ~path: ".resource.url" config ^/ data_fname;
      resource_type = string_of_sexp_path ~path: ".resource.resource_type" config;
    } in
    let resource_doc_metadata = {
      description = Metadata_description;
      rformat = "application/xml";
      url = string_of_sexp_path ~path: ".documentation.url" config;
      resource_type =
        string_of_sexp_path ~path: ".documentation.resource_type" config;
    } in
    let resource_doc_valrep = {
      description = Validation_report;
      rformat = "application/pdf";
      url = string_of_sexp_path ~path: ".validation_report.url" config;
      resource_type =
        string_of_sexp_path ~path: ".validation_report.resource_type" config;
    } in
    let license = {
      common_fields =
        {license_title =
           string_of_sexp_path ~path: ".license.title" config};
      license_url = string_of_sexp_path ~path: ".license.url" config;
      license_id = string_of_sexp_path ~path: ".license.id" config;
    } in
    let dataset =
      {
        license;
        contact_name =
          string_of_sexp_path ~path: ".dataset.contact_name" config;
        contact_email =
          string_of_sexp_path ~path: ".dataset.contact_email" config;
        contact_telephone =
          string_of_sexp_path ~path: ".dataset.contact_telephone" config;
        common_fields = {
          identifier = id |> String.uppercase;
          description =
            string_of_sexp_path ~path: ".dataset.description" config;
          title = unspecified;
        };
        dprivate;
        name = id;
        metadata_language =
          string_of_sexp_path ~path: ".dataset.metadata_language" config;
        language = [];
        url = string_of_sexp_path ~path: ".dataset.url" config ^/ id;
        modified_date = Date.today ~zone: Time.Zone.utc;
        status = [string_of_sexp_path ~path: ".dataset.status" config];
        groups = [{name = string_of_sexp_path ~path: ".dataset.groups.name"
                                              config}];
        owner_org =
          string_of_sexp_path ~path: ".dataset.owner_organisation" config;
        rtype = "dataset";
        resources = [Data, resource_data;
                     Metadata_description, resource_doc_metadata;
                     Validation_report, resource_doc_valrep];
      } in
    let sanitize datum =
      Re2.rewrite_exn (Re2.create_exn {|[\n\t]|}) ~template: " " datum
      |> Re2.rewrite_exn (Re2.create_exn "\"") ~template: "'" in
    begin
      Xml.parse_file fname |> Xml.children |> List.iter ~f: (function
          | Xml.Element ("identificationInfo", _, data) ->
            List.iter data ~f: (
              function
              | Xml.Element ("resourceName", _, Xml.PCData datum :: _) ->
                dataset.common_fields.title <- "(ELRC) " ^ datum
              | Xml.Element ("description", _, Xml.PCData datum :: _) ->
                dataset.common_fields.description <-
                  dataset.common_fields.description ^ sanitize datum
              | _ -> ())
          | Xml.Element ("metadataInfo", _, data) ->
            List.iter data ~f: (
              function
              | Xml.Element ("metadataLastDateUpdated", _, Xml.PCData datum :: _)
                -> dataset.modified_date <- Date.of_string datum
              | _ -> ()
            )
          | Xml.Element ("resourceComponentType", _, data) -> List.iter data ~f:(
              function
              | Xml.Element ("corpusInfo", _, data)
              | Xml.Element ("lexicalConceptualResourceInfo", _, data) ->
                List.iter data ~f: (
                  function
                  | Xml.Element ("corpusMediaType", _, data)
                  | Xml.Element ("lexicalConceptualResourceMediaType", _, data)
                      -> List.iter data ~f: (
                      function
                      | Xml.Element ("corpusTextInfo", _, data)
                      | Xml.Element ("lexicalConceptualResourceTextInfo", _,
                                     data) -> List.iter data ~f: (
                          function
                          | Xml.Element ("textFormatInfo", _, data) ->
                            List.iter data ~f: (
                              function
                              | Xml.Element
                                  ("mimeType", _, Xml.PCData data :: _) ->
                                let resource =
                                  (List.nth_exn
                                     dataset.resources 0)
                                  |> snd in
                                if resource.rformat = unspecified then
                                  resource.rformat <- data
                                else
                                  resource.rformat <-
                                    resource.rformat ^ ", " ^ data
                              | _ -> ()
                            )
                          | Xml.Element ("languageInfo", _, data) ->
                            List.iter data ~f: (
                              function
                              | Xml.Element ("languageId", _,
                                             Xml.PCData data :: _) ->
                                begin
                                  match languages with
                                  | None -> ()
                                  | Some lang_file ->
                                    let language_map =
                                      map_language_codes ~config lang_file in
                                    let auth_code =
                                      List.Assoc.find_exn
                                        ~equal: String.equal
                                        language_map
                                        data in
                                    dataset.language <-
                                      auth_code :: dataset.language
                                end
                              | _ -> ())
                          | _ -> ()
                        )
                      | _ -> ()
                    )
                  | _ -> ()
                )
              | _ -> ()
            )
          | _ -> ()
        );
      dataset
    end

  let jsonize_dataset dataset =
    let open Netencoding.Url in
    `Assoc [("contact_name", `String (encode dataset.contact_name));
            ("contact_email", `String (encode dataset.contact_email));
            ("contact_telephone", `String (encode dataset.contact_telephone));
            ("name", `String (encode dataset.name));
            ("type", `String (encode dataset.rtype));
            ("title", `String (encode dataset.common_fields.title));
            ("description", `String (encode dataset.common_fields.description));
            ("url", `String dataset.url);
            ("modified_date", `String (dataset.modified_date |> Date.to_string));
            ("license_id", `String dataset.license.license_id);
            ("license_title",
             `String (encode dataset.license.common_fields.license_title));
            ("license_url", `String dataset.license.license_url);
            ("status", `List [`String (dataset.status |> List.hd_exn)]);
            ("owner_org", `String (encode dataset.owner_org));
            ("language", `List (List.map dataset.language ~f: (
                 fun lang -> `String lang)));
            ("private", `Bool dataset.dprivate);
            ("identifier", `String dataset.common_fields.identifier);
            ("groups",
             `List [`Assoc [("name",
                             `String (dataset.groups |> List.hd_exn).name)]]);
            ("metadata_language", `String dataset.metadata_language)]
    |> Yojson.Basic.to_string

  let jsonize_resource dataset resource_type =
    let resource = List.Assoc.find ~equal: (=) dataset.resources resource_type in
    begin
      match resource with
      | None -> `Assoc []
      | Some res ->
        let fname_coda =
          Variants_of_rdescription_t.to_name res.description ^ 
          begin
            match resource_type with
            | Data -> ".zip"
            | Metadata_description -> ".xml"
            | Validation_report -> ".pdf"
          end in
        let open Netencoding.Url in
        `Assoc [("description",
                 `String (encode ((Variants_of_rdescription_t.to_name res.description
                           |> Re2.rewrite_exn (Re2.create_exn "_") ~template: " ")
                          ^ (if resource_type = Data then
                               " archive containing files in the following \
                                formats: " ^ res.rformat
                             else ""))));
                ("format", `String (encode
                                      (if resource_type = Data then
                                         "application/zip"
                                       else res.rformat)));
                ("package_id", `String (encode dataset.name));
                ("url", `String (if resource_type = Data then res.url
                                 else  dataset.url ^/ fname_coda));
                ("resource_type", `String res.resource_type)]
    end
    |> Yojson.Basic.to_string

  (** Curl-based runner based on subprocessing curl, as the bindings to libcurl
   *  are too low-level. Return entity ID if created successfully.*)

  let update_entry
      ~config ~etype ?(update_type=`Create) ~debug auth_token json_data =
    let ckan_root = string_of_sexp_path ~path: ".portal_root" config in
    let prog = "curl" in
    let etype_url =
      match etype with
      | `Dataset -> "package"
      | `Resource -> "resource" in
    let arguments = [
      "-s";
      "-H";
      "Authorization: " ^ auth_token;
      ckan_root ^/ "api/action" ^/ etype_url ^
      (match update_type with
       | `Create -> "_create"
       | `Update ->
         begin
           match etype with
           | `Dataset -> "_update"
           | `Resource -> "_create"
         end);
      "-d"; json_data] in
    if debug = false then
      let out_json =
        Shell.run_full prog arguments
        |> Yojson.Basic.from_string in
      match Yojson.Basic.Util.member "success" out_json with
      | `Bool true ->
        begin
          match Yojson.Basic.Util.member "result" out_json
                |> Yojson.Basic.Util.member "id" with
          | `String id -> Some id
          | _ -> raise (Elrc_to_odp_exn "Ill-formed JSON")
        end
      | `Bool false -> None
      | _ -> raise (Elrc_to_odp_exn "Ill-formed JSON response")
    else
      begin
        print_endline (prog ^ " " ^ String.concat arguments ~sep: " ");
        Some "DEBUG-ID"
      end

  let update_resource
      ?json_data ?upload_file ~config ~debug auth_token resource_id =
    let ckan_root = string_of_sexp_path ~path: ".portal_root" config in
    let parameters =
      match json_data, upload_file with
      | None, None | Some _, Some _ ->
        raise (Elrc_to_odp_exn
                 "Cannot update resource with both file upload and raw JSON \
                  data")
      | Some data, None -> ["-d"; data]
      | None, Some file_name -> [
          "--form";
          "id=" ^ resource_id;
          "--form";
          "upload=@\"" ^ file_name ^ "\""] in
    let prog = "curl" in
    let arguments = [
      "-s";
      "-H";
      "Authorization: " ^ auth_token;
      ckan_root ^/ "api/action/resource_update"] @ parameters in
    if debug = false then
      let out_json =
        Shell.run_full prog arguments
        |> Yojson.Basic.from_string in
      match Yojson.Basic.Util.member "success" out_json with
      | `Bool value -> value
      | _ -> raise (Elrc_to_odp_exn "Ill-formed JSON response")
    else
      begin
        print_endline (prog ^ " " ^ String.concat arguments ~sep: " ");
        true
      end

  (** Main dataset poster driver *)
  let post_dataset
      ?(debug=true) ~config ~dprivate ~update_type ~upload_data language_file
      directory auth_token =
    let dir_contents = Sys.ls_dir directory in
    let md_file =
      Filename.realpath directory ^/
      (List.filter dir_contents ~f: (Fn.flip Filename.check_suffix "_md.xml")
       |> List.hd_exn)
    in
    let data_file =
      Filename.realpath directory ^/
      (List.filter dir_contents ~f: (Fn.flip Filename.check_suffix "_dataset.zip")
       |> List.hd_exn)
    in
    let valrep_file =
      Filename.realpath directory ^/
      (List.filter dir_contents ~f: (Fn.flip Filename.check_suffix "_VALREP.pdf")
       |> List.hd_exn)
    in
    let dataset =
      get_resource_from_elrc_file ~config ~dprivate
                                  ~languages: language_file md_file in
    let dataset_json = jsonize_dataset dataset in
    let resource_json = jsonize_resource dataset Data in
    let metadata_json = jsonize_resource dataset Metadata_description in
    let valrep_json = jsonize_resource dataset Validation_report in
    begin
      (* actual curl-based interactions.*)
      match update_entry ~config ~etype: `Dataset ~update_type ~debug auth_token
                         dataset_json with
      | None -> print_endline "Failure to create dataset"
      | Some _ -> List.iteri [
          resource_json, data_file;
          metadata_json, md_file;
          valrep_json, valrep_file] ~f:(fun i ->
          function
          | json, file ->
            begin
              match update_entry ~config ~etype: `Resource ~update_type ~debug
                                 auth_token json with
              | None -> print_endline "Failure to create resource"
              | Some resource_id when upload_data = true || i > 0 ->
                begin
                  match update_resource
                          ~config ~upload_file: file
                          ~debug auth_token resource_id
                  with
                  | false -> print_endline "Failure to update resource"
                  | true when debug = true -> print_endline "Debug"
                  | true -> print_endline "Success"
                end
              | Some _ ->
                print_endline "As requested, the resource is not being updated"
          end
        )
    end
end

let command =
  Command.basic_spec
    ~summary: "Automatically submit ELRC data + metadata to the Open Data Portal"
    ~readme: (fun () -> "=== Copyright © 2018 ELDA - All rights reserved ===\n")
    Command.Spec.(
      empty
      +> flag "--resource-directory" (required string)
        ~doc: " Resource data and metadata directory"
      +> flag "--authorisation-token" (required string)
        ~doc: "Authorisation token for submitting data to the Open Data Portal"
      +> flag "--languages-file" (required string)
        ~doc: " File containing language codes"
      +> flag "--nodebug" no_arg
        ~doc: " Do not launch in debug mode: launch in production mode, push \
               the data and metadata"
      +> flag "--update" no_arg
        ~doc: " If provided, update package instead of creating"
      +> flag "--private" (optional_with_default true bool)
        ~doc: " Submit data as private (true by default)"
      +> flag "--upload-data" no_arg
        ~doc: " Upload data to the Open Data Portal"
      +> flag "--configuration-file" (required string)
        ~doc: " Configuration file, in the S-expression format"
    )
    (fun directory auth_token language_file nodebug uptype dprivate
         upload_data config_file () ->
       let update_type =
           match uptype with
           | false -> `Create
           | true -> `Update
       in
       let open Api in
       begin
         Printf.sprintf "> Processing dataset from directory %s..." directory
         |> print_endline;
         let config = Sexp.load_sexp config_file in
         post_dataset
           ~config ~debug: (not nodebug) ~upload_data ~dprivate ~update_type
           language_file directory auth_token;
         print_endline "< Done"
     end
    )

let () = Command.run ~version: "1.0" ~build_info: "ELDA on Debian" command
