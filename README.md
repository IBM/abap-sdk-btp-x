<!--
  ------------------------------------------------------------------------
  Copyright 2020, 2021 IBM Corp. All Rights Reserved.
  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at
      http://www.apache.org/licenses/LICENSE-2.0
  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
  ------------------------------------------------------------------------
-->

# ABAP SDK for IBM watsonx, using SAP BTP ABAP Environment

[![License](https://img.shields.io/badge/License-Apache2-blue.svg)](https://www.apache.org/licenses/LICENSE-2.0)

ABAP<sup>®</sup> client library to use the [watsonx.ai Runtime][wml] (formerly 
Watson Machine Learning) and the [watsonx.ai][wxai] APIs.

This is a community SDK written by ABAPers for the ABAPers in the Open
Source community, to provide easy usage of IBM<sup>®</sup>
watsonx<sup>™</sup> services in innovation initiatives using the
SAP<sup>®</sup> Business Technology Platform ABAP Environment. It is
the choice and responsibility of application developers how this
community SDK is used.

Additionally, as the ABAP SDK is a community release it is not updated
with the same schedule as IBM-supported SDKs. Please see more
information in [Support and Feedback](#support-and-feedback).

# Choose ABAP SDK release for the applicable ABAP Runtime

| [abap-sdk-nwas-x](https://github.com/IBM/abap-sdk-nwas-x) | **abap-sdk-btp-x** |
|---|---|
| for SAP NetWeaver<sup>®</sup> AS ABAP 7.50+ | for SAP BTP ABAP Environment |
| tested on SAP Business Suite and SAP S/4HANA on-premises edition | tested with SAP BTP ABAP Environment 2405 |
|  | `this repository` |

---

<details>
  <summary>Table of Contents</summary>

- [Announcements](#announcements)
  - [Version 1.3.0 released](#version-130-released)
- [Before you begin](#before-you-begin)
- [Installation](#installation)
- [SSL Certificates](#ssl-certificates)
- [Credentials](#credentials)
- [Configuration table](#configuration-table)
- [IAM Authentication](#iam-authentication)
- [Usage](#usage)
  - [Examples](#examples)
- [API Reference](#api-reference)
- [Support and Feedback](#support-and-feedback)
- [License](#license)

</details>

## Announcements

### Version 1.3.0 released

Version v1.3.0 of the SDK has been released - see what's changed in
the [migration guide](MIGRATION-V1.3.0.md).

## Before you begin

* You need an [IBM Cloud][ibm_cloud_onboarding] account.

## Installation

The client library is provided as abapGit repository. Proceed as
follows to clone the ABAP SDK code to your SAP system.

1. Install the [abapGit Plug-In][abapgit_plugin] for ADT.
2. Use abapGit to clone the ABAP SDK Git repository into your SAP
   system.
3. Assign the ABAP SDK to the package `ZIBMX` when performing setup of
   the abapGit repository.

## SSL Certificates

Communication between SAP and the IBM Cloud is secured by the
Transport Layer Security (TLS) protocol, also known as Secure Sockets
Layer (SSL). SSL requires certificates that must be stored in the SAP
BTP Certificate Trust List, which can be maintained
through the SAP BTP ABAP Environment dashboard. Ensure
that the *DigiCert Global Root CA* certificate is installed.

## Credentials

Before you can access a specific service from your SAP system, you
must create a service instance in the IBM Cloud and
[obtain an API key](https://cloud.ibm.com/docs/account?topic=account-userapikey&interface=ui).
The API key can be shared among multiple end users of the SAP system, but
you must ensure that the selected plan is sufficient to support the expected
number of calls or the expected data volume.

All currently supported services support IAM authentication (see
below). Service credentials consist of an API key and a URL. Both the
API key and the URL are character values that need to be provided as parameters
`i_apikey` and `i_url` to method `zcl_ibmx_service_ext=>get_instance`.

You can store the values with your application, but it is suggested to
do that in an encrypted format. Using cloud services usually creates
costs based on usage for the owner of the service instance, and anyone
with the credentials can use the service instance at the owner's
expenses. If you want to distribute the costs over multiple cost
centers, you need to create a service instance and provide service
credentials for each cost center separately.

## Configuration table

Service credentials and other parameters that must be specified at
watsonx service wrapper ABAP class instantiation can also be provided
in table `ZIBMX_CONFIG`. This table has three keys:

| Table Key    | Description                                                                                                                            |
|:------------ |:-------------------------------------------------------------------------------------------------------------------------------------- |
| SERVICE      | The ABAP class name without prefix ZCL_IBMX_                                                                                           |
| INSTANCE_UID | ID chosen by application developer that must be provided by application as parameter to method `zcl_ibmx_service_ext=>get_instance()`  |
| PARAM        | The parameter name                                                                                                                     |

<details>
  <summary>List of configuration parameters in table ZIBMX_CONFIG</summary>

| Parameter Name    | Default Value     | Description                                                                 |
|:----------------- |:----------------- |:--------------------------------------------------------------------------- |
| URL               | service-dependent | watsonx service url                                                         |
| APIKEY            |                   | watsonx service API keys                                                    |
| AUTH_NAME         | service-dependent | Authorization, `IAM` or `basicAuth`                                         |

</details>

## IAM Authentication

Identity and Access Management (IAM) is a bearer-token based
authentication method. Token management is either performed by the
ABAP SDK or must be implemented by the SDK user.<br/> If a value for
`i_apikey` is provided by the caller in method
`zcl_ibmx_service_ext=>get_instance()`, the ABAP SDK generates a
bearer-token under the cover when needed and refreshes it when it is
about to expire.<br/> If `i_apikey` is not provided for method
`zcl_ibmx_service_ext=>get_instance()`, the ABAP SDK user must
implement an individual token management. Before a service method is
called the first time, a valid bearer-token must be provided to the
watsonx service wrapper ABAP class instance as follows:

```abap
  lo_service_class->set_bearer_token( i_bearer_token = '...' ).
```

Afterwards, service methods can be called as long as the provided
token is valid. When the token has expired, the method above must be
called again with a new (refreshed) bearer-token as parameter.

## Usage

The client library is delivered as package *ZIBMX*. Once the Git
repository has been cloned to the SAP system, a watsonx service
instance is wrapped by an ABAP class instance.<br> The following
watsonx services are currently supported:

| Service                        | ABAP Class Name                     |
|:------------------------------ |:----------------------------------- |
| watsonx.ai Runtime             | ZCL_IBMX_WML_V4                     |
| watsonx.ai                     | ZCL_IBMX_WATSONX_AI_ML_V1           |

Using the client library requires two steps:

1. Create an instance of the watsonx service wrapper ABAP class by
   calling method `zcl_ibmx_service_ext=>get_instance`.

   ```abap
     data:
       lo_service_class type <ABAP Class Name>.
   
     zcl_ibmx_service_ext=>get_instance(
       exporting
         i_url      = <url>
         i_apikey   = <api key>
         ...
       importing
         eo_instance = lo_service_class ).
   ```

   > **Note**  
   > Depending on the API operation, the `<url>` parameter may vary as
   > described in
   > [Endpoint URLs](https://cloud.ibm.com/apidocs/watsonx-ai#endpoint-url).
   > If you use API operations with different endpoint URLs in the same
   > program, you need to create an instance of the watsonx service
   > wrapper ABAP class for each endpoint URL and use the instances
   > accordingly in your program.

2. Call the watsonx service API endpoint by invoking the corresponding
   class method.

   ```abap
     try.
         lo_service_class->method(
           exporting
             is_input = ...
           importing
             es_output = ... ).
       catch zcx_ibmx_service_exception into data(lo_service_exception).
         ...
     endtry.
   ```

### Examples

<details>
  <summary>watsonx.ai Runtime Example</summary>

```abap
*  Z_WML_DEMO
*  This sample code deploys arbitrary Python code as Python function on a watsonx deployment space
*    and calls the deployed function.
*    It also returns a CURL command that can be called to invoke the function endpoint url.
*
*  Copy this code into
*    - an ABAP report and remove all comment prefixes "[PRG]
*    - a Console Application (RAP) and remove all comment prefixes "[RAP]
*  Adjust credentials and run code.

  constants:
    " Prerequisite: watsonx deployment space must be created and id specified here:
    c_url           type string value `https://eu-de.ml.cloud.ibm.com`,  " <- ADJUST
    c_apikey        type string value `X7ZiqsaYv9X...OAYFxxhAno`,        " <- ADJUST
    c_space_id      type string value `78f395be-...-a1d1abb8644b`,       " <- ADJUST
    c_function_name type string value `demo-python-function`,
    c_serving_name  type string value `abapsdk_test`,
    c_version       type string value `2023-07-07`.

  " convert Python function code to GZIP file (ZIP with file header 0x04034b50 is not supported)
  try.
      data(lx_code) = zcl_ibmx_service=>convert_string_to_utf8(
        i_string =
          `def score(input_data):` && cl_abap_char_utilities=>newline &&
          `    # some Python code` && cl_abap_char_utilities=>newline &&
          `    return {'predictions': [{'values': [['SUCCESS (ABAP SDK used for deployment)']]}]}`
      ).

      cl_abap_gzip=>compress_binary_with_header(
        exporting
          raw_in = lx_code
        importing
          gzip_out = data(lx_gzipfile)
      ).
    catch cx_root into data(lo_zip_exception).
      "[RAP] out->write( lo_zip_exception->get_longtext(  ) ). exit.
      "[PRG] message lo_zip_exception type 'E'.
  endtry.

  " instantiate watsonx.ai Runtime wrapper class (explicit type declaration is required)
  data: lo_wml type ref to zcl_ibmx_wml_v4.
  zcl_ibmx_service_ext=>get_instance(
    exporting
      i_url     = c_url
      i_apikey  = c_apikey
      i_version = c_version
    importing
      eo_instance = lo_wml ).

  " create Python function as asset in watsonx deployment space
  try.
      lo_wml->functions_create(
        exporting
          i_functionentityrequest = value zcl_ibmx_wml_v4=>t_function_entity_request(
            name     = c_function_name
            space_id = c_space_id
            type     = `Python`
            software_spec = value zcl_ibmx_wml_v4=>t_software_spec_rel(
              name = `runtime-23.1-py3.10`
            )
          )
        importing
          e_response = data(ls_result_create)
      ).
    catch zcx_ibmx_service_exception into data(lo_create_exception).
      "[RAP] out->write( lo_create_exception->get_longtext(  ) ). exit.
      "[PRG] message lo_create_exception type 'E'.
  endtry.
  data(lv_function_id) = ls_result_create-metadata-id.

  " upload Python function code
  try.
     lo_wml->functions_upload_code(
       exporting
         i_function_id = lv_function_id
         i_space_id    = c_space_id
         i_upload_code = lx_gzipfile
         i_contenttype = 'application/gzip'
      importing
        e_response = data(ls_result_upload)
     ).
    catch zcx_ibmx_service_exception into data(lo_upload_exception).
      "[RAP] out->write( lo_upload_exception->get_longtext(  ) ). exit.
      "[PRG] message lo_upload_exception type 'E'.
  endtry.

  " deploy Python function
  try.
      lo_wml->deployments_create(
        exporting
          i_deploymententityrequest = value zcl_ibmx_wml_v4=>t_deployment_entity_request(
            space_id = c_space_id
            name     = `Deployment for ` && c_function_name
            asset = value zcl_ibmx_wml_v4=>t_rel(
              id = lv_function_id
            )
            hardware_spec = value zcl_ibmx_wml_v4=>t_hardware_spec(
              name = 'S'
            )
            online = value zcl_ibmx_wml_v4=>t_online_request(
              parameters = value zcl_ibmx_wml_v4=>t_online_parameters(
                serving_name = c_serving_name
              )
            )
          )
        importing
          e_response = data(ls_deployment_result)
       ).
    catch zcx_ibmx_service_exception into data(lo_deployment_exception).
      " http status 400 (bad request) -> delete existing deployment, if exists, and retry
      "[RAP] out->write( lo_deployment_exception->get_longtext(  ) ). exit.
      "[PRG] message lo_deployment_exception type 'E'.
  endtry.
  data(lv_deployment_id) = ls_deployment_result-metadata-id.

  " wait until deployment is initialized
  do 20 times.
    try.
        lo_wml->deployments_get(
          exporting
            i_space_id      = c_space_id
            i_deployment_id = lv_deployment_id
          importing
            e_response = data(ls_deployment)
        ).
      catch zcx_ibmx_service_exception into data(lo_deployment_get_exception).
        "[RAP] out->write( lo_deployment_get_exception->get_longtext(  ) ). exit.
        "[PRG] message lo_deployment_get_exception type 'E'.
    endtry.
    if not ls_deployment-entity-status-state eq 'initializing'.
      exit.
    endif.
    wait up to 5 seconds.
  enddo.

  " call Python function endpoint with dummy input data
  try.
      lo_wml->deployments_compute_predict(
        exporting
          i_deployment_id   = lv_deployment_id
          i_syncscoringdata = value zcl_ibmx_wml_v4=>t_sync_scoring_data(
            input_data = value #(
              ( value zcl_ibmx_wml_v4=>t_sync_scoring_data_item(
                  fields = value #( ( `dummy` ) )
                  values = value #( ( value #( ( ref #( `dummy` ) ) ) ) )
               ) ) ) )
        importing
          e_response = data(ls_function_response)
      ).
    catch zcx_ibmx_service_exception into data(lo_predict_exception).
      "[RAP] out->write( lo_predict_exception->get_longtext(  ) ). exit.
      "[PRG] message lo_predict_exception type 'E'.
  endtry.

  " extract message from function response
  try.
      data(lr_msg) = ls_function_response-predictions[ 1 ]-values[ 1 ][ 1 ].
      assign lr_msg->* to field-symbol(<lfs_msg>).
    catch cx_sy_itab_line_not_found into data(lo_line_not_found_exception).
      "[RAP] out->write( lo_line_not_found_exception->get_longtext(  ) ). exit.
      "[PRG] message lo_line_not_found_exception type 'E'.
  endtry.

  " compile CURL command to test deployment endpoint
  data(lv_curl_command) =
    `curl -X POST -H "Authorization: Bearer ${IAM_ACCESS_TOKEN}" `
    && `-d '{ "input_data": [{"fields": ["dummy"], "values": [["dummy"]]}] }' `
    && `"` && ls_deployment-entity-status-inference[ 1 ]-url && `?version=` && c_version && `"`.

  " write success message
  data(lv_msg) = `Received message "` && <lfs_msg> && `" from deployed Python function. Try CURL command below.`.
  "[RAP] out->write( lv_msg ).
  "[RAP] out->write( lv_curl_command ).
  "[PRG] write: lv_msg.
  "[PRG] write: / lv_curl_command.
```

</details>

<details>
  <summary>watsonx.ai Example</summary>

```abap
*  Z_WATSONX_AI_DEMO
*  This sample code runs text generation with a watsonx.ai foundation model.
*
*  Copy this code into
*    - an ABAP report and remove all comment prefixes "[PRG]
*    - a Console Application (RAP) and remove all comment prefixes "[RAP]
*  Adjust credentials and run code.

  " instantiate watsonx.ai wrapper class (explicit type declaration is required)
  data: lo_watsonx_ai type ref to zcl_ibmx_watsonx_ai_ml_v1.
  zcl_ibmx_service_ext=>get_instance(
    exporting
      i_url     = 'https://eu-de.ml.cloud.ibm.com'   " <- ADJUST
      i_apikey  = 'X7ZiqsanCt1sYv...ENYbFFfOAtAno'   " <- ADJUST
      i_version = '2023-05-29'
    importing
      eo_instance = lo_watsonx_ai ).

  " run text generation
  try.
      data(lv_prompt) = `The highest mountain in Europe is`.
      lo_watsonx_ai->text_generation(
        exporting
          i_textgenrequest = value zcl_ibmx_watsonx_ai_ml_v1=>t_text_gen_request(

            " prompt
            input = lv_prompt

            " model parameters
            model_id   = 'ibm/granite-13b-chat-v2'
            parameters = value zcl_ibmx_watsonx_ai_ml_v1=>t_text_gen_parameters(
              decoding_method    = 'greedy'
              max_new_tokens     = 20
              repetition_penalty = '1.05'
              stop_sequences = value #( ( `. ` ) )  " stop after first sentence
            )

            project_id = '3e606d0c-...-4e459b7c38'   " <- ADJUST
          )
        importing
          e_response = data(ls_generated_document) ).
    catch zcx_ibmx_service_exception into data(lo_service_exception).
      "[RAP] out->write( lo_service_exception->get_longtext(  ) ). exit.
      "[PRG] message lo_service_exception type 'E'.
  endtry.

  " display generated text
  "[RAP] out->write( lv_prompt ).
  "[PRG] write lv_prompt.
  loop at ls_generated_document-results into data(ls_result).
  "[RAP]   out->write( ls_result-generated_text ).
  "[PRG]   write / ls_result-generated_text.
  endloop.
```

</details>

## API Reference

GitHub Pages contain the [ABAP Client Library for watsonx API Reference](https://IBM.github.io/abap-sdk-nwas-x/).

## Support and Feedback

The ABAP SDK is a community SDK for IBM watsonx, created by the IBM watsonx
development community and SAP's ABAP development community – written by
ABAPers from IBM Systems. Therefore, as a community release it is not
updated with the same schedule as IBM-supported SDKs and does not include
support by IBM.

The code in this repository is generated from the OpenAPI specs of the
supported services. Hence, pull requests must not be integrated directly
into the code. If you would like to request a new feature or report a
bug, please raise an issue instead of sending a pull request.

## License

This library is licensed under the [Apache 2.0 license][license].

[wml]: https://cloud.ibm.com/apidocs/machine-learning
[wxai]: https://cloud.ibm.com/apidocs/watsonx-ai
[ibm_cloud]: https://cloud.ibm.com/
[ibm_cloud_onboarding]: https://cloud.ibm.com/registration?target=/developer/watson&cm_sp=WatsonPlatform-WatsonServices-_-OnPageNavLink-IBMWatson_SDKs-_-ABAP
[license]: http://www.apache.org/licenses/LICENSE-2.0
[abapgit]: https://github.com/larshp/abapGit
[abapgit_docs]: https://docs.abapgit.org/
[abapgit_plugin]: https://eclipse.abapgit.org/updatesite/
