ELRC-Share to Open Data Portal (Meta-)Data Exporter
===================================================

Synopsis
--------

This tool allows users to export metadata and optionally data from the
[ELRC-Share](https://www.elrc-share.eu/) repository to the EU [Open Data
Portal](https://data.europa.eu/euodp/en/home).

More specifically, the tool takes a dataset dump from the ELRC-Share
repository and automatically uploads the associated metadata, the
validation report of the dataset, and, optionally the data itself, to
the Open Data Portal.

From a technical standpoint, the tool uses the XML description of the
dataset metadata from the ELRC-Share repository, along with the
validation report and uploads them to the Open Data Portal via the JSON
REST API the latter supports.

The tool is distributed as a command-line tool, is developed in the
[OCaml](http://ocaml.org) programming language (version 4.05) and works
under the Linux operating systems.

Installing
----------

In order to install the tool, it suffices to have
[OCaml](http://ocaml.org) and its package manager,
[OPAM](http://opam.ocaml.org), installed, then to proceed, in a shell,
to:

    opam init

    opam switch 4.05.0

    opam update

    opam pin add <https://path.to.application.on.github> -y

If this command is successful, the tool can be used as:

    elrc_to_odp -help

in order to see the available options, which are documented below.

Usage
-----

### Data Inputs

The tool uses the following data inputs:

-   a configuration file, formatted as an
    [S-expression](https://en.wikipedia.org/wiki/S-expression); the
    `config.sample` file documents the required format.
-   a file specifying language codes, formatted in XML, as, for example:

``` {.sourceCode .xml}
<languages xmlns:xsi="http://www.w3_org/2001/XMLSchema-instance">
  <record>
    <iso-639-1>en</iso-639-1>
    <authority-code>ENG</authority-code>
  </record>
  ...
</languages>
```

> This file can be generated e.g. from the [Language Name Authority
> List](https://data.europa.eu/euodp/data/dataset/language) resource
> present on the European Union Open Data Portal.

-   the path to the directory containing the resource data and metadata,
    as extracted from the ELRC-Share repository. Such a directory is
    expected to be called `ELRC_<id>_<resource_name>` and to contain at
    least three files:
    -   `ELRC_<id>_<resource_name>_md.xml`, containing the dataset
        metadata;
    -   `ELRC_<id>_<resource_name>_VALREP.pdf`, containing the dataset
        validation report;
    -   `ELRC_<id>_<resource_name>_dataset.zip`, containing the dataset
        data, in case the data is expected to be uploaded to the Open
        Data Portal as well.

### Command-line Parameters

The full listing of the command-line interface of the tool is given
below:

    --authorisation-token: Authorisation token for submitting data to the Open Data Portal via the JSON REST API.

    --configuration-file:  Configuration file, in the S-expression format, as described above.

    --languages-file: File containing language codes, as described above.

    --resource-directory: Resource data and metadata directory, as described above.

    [--nodebug]: If provided, do not launch in debug mode; launch in production mode, push the data and metadata. Otherwise, only print the JSON REST API calls, for debugging purposes.

    [--private]: Submit data as private (true by default). To be set to false only when one wishes to save the data as "published" so that it is visible to all users of the Open Data Portal.

    [--update]: If provided, update package instead of creating.

    [--upload-data]: If provided, upload data to the Open Data Portal.

### Example

Example of a command for publishing an entry to the Open Data Portal:

    elrc_to_odp --authorisation-token "XXX-YYY" --languages-file languages.xml 
    --configuration-file config.txt --resource-directory
    /path/to/ELRC_310_English-Bulgarian\ Legal\ Terms/ --private false --nodebug
