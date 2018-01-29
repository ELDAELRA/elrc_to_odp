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

(** Common code to ELRC / ODP programs. *)

open Core
open Re2.Std

exception Elrc_to_odp_exn of string

(** Licensing information shared between elrc_to_odp and elrc_to_elra
    programs.*)
type license_common_t = {
  license_title: string
}[@@unboxed]

(** Dataset information shared between elrc_to_odp and elrc_to_elra programs.*)
type dataset_common_t = {
  (* ELRC ^ "_" ^ resource number *)
  identifier: string;
  mutable description: string;
  mutable title: string;
}

(** Type used by all ELRC / ODP programs. *)
type rdescription_t =
  | Data
  | Validation_report
  | Metadata_description [@@deriving variants]

(** Type used by all ELRC / ODP programs. *)
type resource_t = {
  (* http://www.w3.org/TR/vocab-dcat#Download for data
   * or http://open-data.europa.eu/kos/documentation-type/RelatedDocumentation
   * for documentation *)
  resource_type: string;
  (* individual item description: data or documentation mutable, since data
   * description + format *)
  mutable description: rdescription_t;
  (* mime type of the individual item *)
  mutable rformat: string;
  mutable url: string;
}

(** Type used by all ELRC / ODP programs. *)
type keyword_t = {
  name: string
} [@@unboxed]

(** Type used by all ELRC / ODP programs. *)
type group_t = {
  name: string;
} [@@unboxed]

(** Type used by all ELRC / ODP programs. *)
type language_t = {
    mutable iso_639_1_code: string;
    mutable authority_code: string;
}

let unspecified = "<Unspecified>"

(** Return data file name from metadata file named filename. *)
let generate_data_filename filename =
  Filename.basename filename
  |> Re2.rewrite_exn (Re2.create_exn {|\s|}) ~template: "_"
  |> Re2.rewrite_exn (Re2.create_exn {|\[\.{3,3}\]_|}) ~template: ""
  |> Fn.flip Filename.chop_suffix "_md.xml"
  |> Fn.flip (^) "_dataset.zip"

(** Return an identifier from metadata file named filename. *)
let generate_data_id filename =
  Filename.basename filename
  |> String.split ~on: '_'
  |> Fn.flip List.take 2
  |> String.concat ~sep: "_"
  |> String.lowercase

(** Return string value of given path in an S-expression. *)
let string_of_sexp_path ~path sexp =
  let path = Sexplib.Path.parse path in
  Sexplib.Path.get ~path sexp
  |> Sexp.to_string
