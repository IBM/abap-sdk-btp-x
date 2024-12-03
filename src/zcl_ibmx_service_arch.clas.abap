* Copyright 2019, 2024 IBM Corp. All Rights Reserved.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
CLASS ZCL_IBMX_SERVICE_ARCH DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES ZIF_IBMX_SERVICE_ARCH .

    TYPES to_http_client TYPE REF TO if_web_http_client .
    TYPES to_rest_request TYPE REF TO if_web_http_request .
    TYPES to_rest_response TYPE REF TO if_web_http_response .
    TYPES to_form_part TYPE REF TO if_web_http_request .
    TYPES:
      BEGIN OF ts_client,
        http    TYPE to_http_client,
        request TYPE to_rest_request,
      END OF ts_client .
    TYPES:
      BEGIN OF ts_http_status,
        code   TYPE n LENGTH 3,
        reason TYPE string,
        json   TYPE string,
      END OF ts_http_status .
    TYPES ts_header TYPE ZIF_IBMX_SERVICE_ARCH~ts_header .
    TYPES tt_header TYPE ZIF_IBMX_SERVICE_ARCH~tt_header .
    TYPES ts_url TYPE ZIF_IBMX_SERVICE_ARCH~ts_url .
    TYPES ts_access_token TYPE ZIF_IBMX_SERVICE_ARCH~ts_access_token .
    TYPES ts_request_prop TYPE ZIF_IBMX_SERVICE_ARCH~ts_request_prop .

    "! <p class="shorttext synchronized" lang="en">Returns the user's time zone.</p>
    "!
    "! @parameter E_TIMEZONE | user's time zone
    "!
    CLASS-METHODS get_timezone
      RETURNING
        VALUE(e_timezone) TYPE ZIF_IBMX_SERVICE_ARCH~ty_timezone .
    "! <p class="shorttext synchronized" lang="en">Returns an ABAP module identifier.</p>
    "!
    "! @parameter E_PROGNAME | ABAP module identifier
    "!
    CLASS-METHODS get_progname
      RETURNING
        VALUE(e_progname) TYPE string .
    "! <p class="shorttext synchronized" lang="en">Decodes base64 encoded data to binary.</p>
    "!
    "! @parameter I_BASE64 | Base64-encoded binary
    "! @parameter E_BINARY | Binary data
    "! @raising ZCX_IBMX_SERVICE_EXCEPTION | Exception being raised in case of an error.
    "!
    CLASS-METHODS base64_decode
      IMPORTING
        !i_base64       TYPE string
      RETURNING
        VALUE(e_binary) TYPE xstring
      RAISING
        ZCX_IBMX_SERVICE_EXCEPTION .
    "! <p class="shorttext synchronized" lang="en">Encodes a string to base64 format.</p>
    "!
    "! @parameter I_UNENCODED | String, unencoded
    "! @parameter E_ENCODED | Base64-encoded string
    "!
    class-methods base64_encode
      importing
        !i_unencoded type string
      returning
        value(e_encoded) type string .
    "! <p class="shorttext synchronized" lang="en">Returns a HTTP/REST client based on an URL.</p>
    "!
    "! @parameter I_URL | URL
    "! @parameter I_REQUEST_PROP | Request parameters
    "! @parameter E_CLIENT | HTTP/REST client
    "! @raising ZCX_IBMX_SERVICE_EXCEPTION | Exception being raised in case of an error.
    "!
    CLASS-METHODS create_client_by_url
      IMPORTING
        !i_url          TYPE string
        !i_request_prop TYPE ts_request_prop
      EXPORTING
        !e_client       TYPE ts_client
      RAISING
        ZCX_IBMX_SERVICE_EXCEPTION .
    "! <p class="shorttext synchronized" lang="en">Returns a HTTP/REST client based on an DESTINATION.</p>
    "!
    "! @parameter I_REQUEST_PROP | Request parameters
    "! @parameter E_CLIENT | HTTP/REST client
    "! @raising ZCX_IBMX_SERVICE_EXCEPTION | Exception being raised in case of an error.
    "!
    CLASS-METHODS create_client_by_destination
      IMPORTING
        !i_request_prop TYPE ts_request_prop
      EXPORTING
        !e_client       TYPE ts_client
      RAISING
        ZCX_IBMX_service_exception .
    "! <p class="shorttext synchronized" lang="en">Returns the default proxy host and port.</p>
    "!
    "! @parameter I_URL | target URL
    "! @parameter E_PROXY_HOST | Proxy host
    "! @parameter E_PROXY_PORT | Proxy port
    "!
    CLASS-METHODS get_default_proxy
      IMPORTING
        !i_url        TYPE ts_url OPTIONAL
      EXPORTING
        !e_proxy_host TYPE string
        !e_proxy_port TYPE string .
    "! <p class="shorttext synchronized" lang="en">Sets request header for basic authentication.</p>
    "!
    "! @parameter I_CLIENT | HTTP/REST client
    "! @parameter I_USERNAME | User name
    "! @parameter I_PASSWORD | Password
    "!
    CLASS-METHODS set_authentication_basic
      IMPORTING
        !i_client   TYPE ts_client
        !i_username TYPE string
        !i_password TYPE string .
    "! <p class="shorttext synchronized" lang="en">Sets a HTTP header.</p>
    "!
    "! @parameter I_CLIENT | HTTP/REST client
    "! @parameter I_NAME | Header field name
    "! @parameter I_VALUE | Header field value
    "!
    CLASS-METHODS set_request_header
      IMPORTING
        !i_client TYPE ts_client
        !i_name   TYPE string
        !i_value  TYPE string .
    "! <p class="shorttext synchronized" lang="en">Sets the URI for a HTTP request.</p>
    "!
    "! @parameter I_CLIENT | HTTP/REST client
    "! @parameter I_URI | URI
    "!
    CLASS-METHODS set_request_uri
      IMPORTING
        !i_client TYPE ts_client
        !i_uri    TYPE string .
    "! <p class="shorttext synchronized" lang="en">Generates a multi-part request body.</p>
    "!
    "! @parameter I_CLIENT | HTTP/REST client
    "! @parameter IT_FORM_PART | Table of form parts
    "! @raising ZCX_IBMX_SERVICE_EXCEPTION | Exception being raised in case of an error.
    "!
    METHODS add_form_part
      IMPORTING
        !i_client     TYPE ts_client
        !it_form_part TYPE ZIF_IBMX_SERVICE_ARCH~tt_form_part
      RAISING
        ZCX_IBMX_SERVICE_EXCEPTION .
    "! <p class="shorttext synchronized" lang="en">Executes a HTTP request.</p>
    "!
    "! @parameter I_CLIENT | HTTP/REST client
    "! @parameter I_METHOD | HTTP method (GET,POST,PUT,DELETE)
    "! @parameter E_RESPONSE | Response of the request
    "! @raising ZCX_IBMX_SERVICE_EXCEPTION | Exception being raised in case of an error.
    "!
    CLASS-METHODS execute
      IMPORTING
        !i_client         TYPE ts_client
        !i_method         TYPE ZIF_IBMX_SERVICE_ARCH~char
      RETURNING
        VALUE(e_response) TYPE to_rest_response
      RAISING
        ZCX_IBMX_SERVICE_EXCEPTION .
    "! <p class="shorttext synchronized" lang="en">Reads character data from a HTTP response.</p>
    "!
    "! @parameter I_RESPONSE | HTTP response
    "! @parameter E_DATA | Character data
    "!
    CLASS-METHODS get_response_string
      IMPORTING
        !i_response   TYPE REF TO if_web_http_response
      RETURNING
        VALUE(e_data) TYPE string .
    "! <p class="shorttext synchronized" lang="en">Set character data for the body of a HTTP request.</p>
    "!
    "! @parameter I_CLIENT | HTTP/REST client
    "! @parameter I_DATA | Character data
    "!
    CLASS-METHODS set_request_body_cdata
      IMPORTING
        !i_client TYPE ts_client
        !i_data   TYPE string .
    "! <p class="shorttext synchronized" lang="en">Set binary data for the body of a HTTP request.</p>
    "!
    "! @parameter I_CLIENT | HTTP/REST client
    "! @parameter I_DATA | Binary data
    "!
    CLASS-METHODS set_request_body_xdata
      IMPORTING
        !i_client TYPE ts_client
        !i_data   TYPE xstring .
    "! <p class="shorttext synchronized" lang="en">Reads binary data from a HTTP response.</p>
    "!
    "! @parameter I_RESPONSE | HTTP response
    "! @parameter E_DATA | Binary data
    "!
    CLASS-METHODS get_response_binary
      IMPORTING
        !i_response   TYPE REF TO if_web_http_response
      RETURNING
        VALUE(e_data) TYPE xstring .
    "! <p class="shorttext synchronized" lang="en">Returns a HTTP response header.</p>
    "!
    "! @parameter I_RESPONSE | HTTP/REST response
    "! @parameter I_HEADER_FIELD | Header field name
    "! @parameter E_VALUE | Header field value
    "!
    CLASS-METHODS get_response_header
      IMPORTING
        !i_response     TYPE to_rest_response
        !i_header_field TYPE string
      RETURNING
        VALUE(e_value)  TYPE string .
    "! <p class="shorttext synchronized" lang="en">Returns the status of a REST response.</p>
    "!
    "! @parameter I_REST_RESPONSE | HTTP/REST response
    "! @parameter E_STATUS | HTTP status
    "!
    CLASS-METHODS get_http_status
      IMPORTING
        !i_rest_response TYPE REF TO if_web_http_response
      RETURNING
        VALUE(e_status)  TYPE ts_http_status .
    "! <p class="shorttext synchronized" lang="en">Converts STRING data to UTF8 encoded XSTRING.</p>
    "!
    "! @parameter I_STRING | STRING data
    "! @parameter E_UTF8 | UTF8-encoded data
    "!
    CLASS-METHODS convert_string_to_utf8
      IMPORTING
        !i_string     TYPE string
      RETURNING
        VALUE(e_utf8) TYPE xstring
      RAISING
        ZCX_IBMX_SERVICE_EXCEPTION .
    "! <p class="shorttext synchronized" lang="en">Finds (and replaces) a regular expression.</p>
    "!
    "! @parameter I_REGEX | Regular expression
    "! @parameter I_WITH | Replacement (if omitted, FIND is performed)
    "! @parameter I_ALL_OCCURRENCES | 'X' if ALL OCCURRENCES OF should be used
    "! @parameter I_IGNORING_CASE | 'X', if IGNORING CASE should be used
    "! @parameter I_IN | String to be searched
    "! @parameter E_OFFSET | Returns position of occurrence
    "! @parameter C_SUBMATCH1 | 1st submatch
    "! @parameter C_SUBMATCH2 | 2nd submatch
    "! @parameter C_SUBMATCH3 | 3rd submatch
    "! @parameter C_IN | String to be searched and returned
    "! @parameter E_SUBRC | sy-subrc of FIND / REPLACE
    "!
    CLASS-METHODS find_regex
      IMPORTING
        !i_regex           TYPE string
        !i_with            TYPE string OPTIONAL
        !i_all_occurrences TYPE ZIF_IBMX_SERVICE_ARCH=>boolean DEFAULT 'X'
        !i_ignoring_case   TYPE ZIF_IBMX_SERVICE_ARCH=>boolean OPTIONAL
        !i_in              TYPE string OPTIONAL
      EXPORTING
        !e_offset          TYPE int4
      CHANGING
        !c_submatch1       TYPE string OPTIONAL
        !c_submatch2       TYPE string OPTIONAL
        !c_submatch3       TYPE string OPTIONAL
        !c_in              TYPE string OPTIONAL
      RETURNING
        VALUE(e_subrc)     TYPE sysubrc .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_IBMX_SERVICE_ARCH IMPLEMENTATION.


  METHOD base64_decode.

    e_binary = cl_web_http_utility=>decode_x_base64( i_base64 ).

*    if sy-subrc <> 0.
*      ZCL_IBMX_SERVICE=>raise_exception( i_msgno = '030' ).  " Decoding of base64 string failed
*    endif.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_IBMX_SERVICE_ARCH=>BASE64_ENCODE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_UNENCODED                    TYPE        STRING
* | [<-()] E_ENCODED                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD base64_encode.

    e_encoded = cl_web_http_utility=>encode_base64(
      exporting
        unencoded = i_unencoded
    ).

  ENDMETHOD.


  METHOD convert_string_to_utf8.

    CALL METHOD cl_web_http_utility=>encode_utf8
      EXPORTING
        unencoded = i_string
      RECEIVING
        encoded   = e_utf8
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc <> 0.
      ZCL_IBMX_SERVICE=>raise_exception( i_text = 'Cannot convert string to UTF-8' )  ##NO_TEXT.
    ENDIF.

  ENDMETHOD.


  METHOD create_client_by_url.

    DATA:
      lv_text TYPE string.

    TRY.
        "create http destination by url
        DATA(lo_http_destination) =
          cl_http_destination_provider=>create_by_url( i_url ).
      CATCH cx_http_dest_provider_error.
    ENDTRY.

    "create HTTP client by destination
    TRY.
        e_client-http = cl_web_http_client_manager=>create_by_http_destination( lo_http_destination ) .
      CATCH cx_web_http_client_error.
        lv_text = `HTTP client cannot be created: ` && lv_text  ##NO_TEXT.
        ZCL_IBMX_SERVICE=>raise_exception( i_text = lv_text ).
    ENDTRY.

    e_client-request = e_client-http->get_http_request( ).

  ENDMETHOD.


  METHOD create_client_by_destination.

    DATA:
      lv_text TYPE string.

    TRY.
        "get http destination
        DATA(lo_http_destination) =
          cl_http_destination_provider=>create_by_destination( i_request_prop-destination ).
      CATCH cx_http_dest_provider_error.
    ENDTRY.

    "create HTTP client by destination
    TRY.
        e_client-http = cl_web_http_client_manager=>create_by_http_destination( lo_http_destination ) .
      CATCH cx_web_http_client_error.
        lv_text = `HTTP client cannot be created: ` && lv_text  ##NO_TEXT.
        ZCL_IBMX_SERVICE=>raise_exception( i_text = lv_text ).
    ENDTRY.

    e_client-request = e_client-http->get_http_request( ).

  ENDMETHOD.


  METHOD execute.

    DATA:
      lo_request   TYPE REF TO if_web_http_request,
      lv_method    TYPE string,
      lv_text      TYPE string,
      lo_exception TYPE REF TO cx_web_http_client_error.

    TRY.
        CASE i_method.
          WHEN ZIF_IBMX_SERVICE_ARCH~c_method_get.
            lv_method = 'GET'.
            e_response = i_client-http->execute( if_web_http_client=>get ).
          WHEN ZIF_IBMX_SERVICE_ARCH~c_method_post.
            lv_method = 'POST'.
            e_response = i_client-http->execute( if_web_http_client=>post ).
          WHEN ZIF_IBMX_SERVICE_ARCH~c_method_put.
            lv_method = 'PUT'.
            e_response = i_client-http->execute( if_web_http_client=>put ).
          WHEN ZIF_IBMX_SERVICE_ARCH~c_method_patch.
            lv_method = 'PATCH'.
            e_response = i_client-http->execute( if_web_http_client=>patch ).
          WHEN ZIF_IBMX_SERVICE_ARCH~c_method_delete.
            lv_method = 'DELETE'.
            e_response = i_client-http->execute( if_web_http_client=>delete ).
          WHEN others.
            lv_method = ZIF_IBMX_SERVICE_ARCH~c_not_supported.
        ENDCASE.

      CATCH cx_web_http_client_error INTO lo_exception.
        lv_text = lo_exception->get_text( ).
        lv_text = `HTTP ` && lv_method && ` request failed: ` && lv_text  ##NO_TEXT.
        ZCL_IBMX_SERVICE=>raise_exception( i_text = lv_text i_previous = lo_exception ).
    ENDTRY.

    IF lv_method EQ ZIF_IBMX_SERVICE_ARCH~c_not_supported.
      ZCL_IBMX_SERVICE=>raise_exception( i_text = ZIF_IBMX_SERVICE_ARCH~c_not_supported ).
    ENDIF.

  ENDMETHOD.


  METHOD add_form_part.

    DATA:
      ls_form_part TYPE ZIF_IBMX_SERVICE_ARCH~ts_form_part,
      lo_part      TYPE REF TO if_web_http_request.

    LOOP AT it_form_part INTO ls_form_part.

      lo_part = i_client-request->add_multipart( ).

      IF NOT ls_form_part-content_type IS INITIAL.
        lo_part->set_header_field( i_name = `Content-Type` i_value = ls_form_part-content_type )  ##NO_TEXT.
      ENDIF.

      IF NOT ls_form_part-content_disposition IS INITIAL.
        lo_part->set_header_field( i_name = `Content-Disposition` i_value = ls_form_part-content_disposition )  ##NO_TEXT.
      ELSE.
        lo_part->set_header_field( i_name = `Content-Disposition` i_value = `form-data; name="unnamed"` )  ##NO_TEXT.
      ENDIF.

      IF NOT ls_form_part-xdata IS INITIAL.
        DATA(lv_length) = xstrlen( ls_form_part-xdata ).
        lo_part->set_binary( i_data = ls_form_part-xdata i_offset = 0 i_length = lv_length ).
      ENDIF.

      IF NOT ls_form_part-cdata IS INITIAL.
        lo_part->set_text( i_text = ls_form_part-cdata ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_default_proxy.

  ENDMETHOD.


  METHOD get_http_status.
    DATA: ls_status TYPE if_web_http_response=>http_status.

    ls_status = i_rest_response->get_status( ).
    e_status-code   = ls_status-code.
    e_status-reason = ls_status-reason.
    TRY.
        e_status-json   = i_rest_response->get_text( ).
      CATCH cx_web_message_error.
        " response may be binary -> ignore
        e_status-json = ''.
    ENDTRY.
  ENDMETHOD.


  METHOD get_progname.

*    e_progname = sy-cprog.

  ENDMETHOD.


  METHOD get_response_binary.

    e_data = i_response->get_binary( ).

  ENDMETHOD.


  METHOD get_response_header.

    e_value = i_response->get_header_field( i_name = i_header_field ).

  ENDMETHOD.


  METHOD get_response_string.

    e_data = i_response->get_text( ).

  ENDMETHOD.


  METHOD get_timezone.

*    e_timezone = sy-zonlo.

  ENDMETHOD.


  METHOD set_authentication_basic.

    i_client-request->set_authorization_basic(
      EXPORTING
        i_username  = i_username
        i_password  = i_password
    ).

  ENDMETHOD.


  METHOD set_request_body_cdata.

    i_client-request->set_text( i_text = i_data ).

  ENDMETHOD.


  METHOD set_request_body_xdata.

    i_client-request->set_binary( i_data = i_data ).

  ENDMETHOD.


  METHOD set_request_header.

    i_client-request->set_header_field( i_name = i_name i_value = i_value ) .

  ENDMETHOD.


  METHOD set_request_uri.

    DATA:
      lo_request TYPE REF TO if_web_http_request.

    i_client-request->set_uri_path( i_uri_path = i_uri ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_IBMX_SERVICE_ARCH=>FIND_REGEX
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_REGEX                        TYPE        STRING
* | [--->] I_WITH                         TYPE        STRING(optional)
* | [--->] I_ALL_OCCURRENCES              TYPE        BOOLEAN (default ='X')
* | [--->] I_IGNORING_CASE                TYPE        BOOLEAN(optional)
* | [--->] I_IN                           TYPE        STRING(optional)
* | [<---] E_OFFSET                       TYPE        INT4
* | [<-->] C_SUBMATCH1                    TYPE        STRING(optional)
* | [<-->] C_SUBMATCH2                    TYPE        STRING(optional)
* | [<-->] C_SUBMATCH3                    TYPE        STRING(optional)
* | [<-->] C_IN                           TYPE        STRING
* | [<-()] E_SUBRC                        TYPE        SYSUBRC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD find_regex.

    DATA:
      l_in TYPE string.

    IF NOT i_with IS SUPPLIED.
      " FIND
      IF i_in IS SUPPLIED.
        l_in = i_in.
      ELSE.
        l_in = c_in.
      ENDIF.
      IF c_submatch3 IS SUPPLIED.
        IF i_ignoring_case EQ 'X'.
          FIND PCRE i_regex IN l_in MATCH OFFSET e_offset IGNORING CASE SUBMATCHES c_submatch1 c_submatch2 c_submatch3.
        ELSE.
          FIND PCRE i_regex IN l_in MATCH OFFSET e_offset SUBMATCHES c_submatch1 c_submatch2 c_submatch3.
        ENDIF.
      ELSEIF c_submatch2 IS SUPPLIED.
        IF i_ignoring_case EQ 'X'.
          FIND PCRE i_regex IN l_in MATCH OFFSET e_offset IGNORING CASE SUBMATCHES c_submatch1 c_submatch2.
        ELSE.
          FIND PCRE i_regex IN l_in MATCH OFFSET e_offset SUBMATCHES c_submatch1 c_submatch2.
        ENDIF.
      ELSEIF c_submatch1 IS SUPPLIED.
        IF i_ignoring_case EQ 'X'.
          FIND PCRE i_regex IN l_in MATCH OFFSET e_offset IGNORING CASE SUBMATCHES c_submatch1.
        ELSE.
          FIND PCRE i_regex IN l_in MATCH OFFSET e_offset SUBMATCHES c_submatch1.
        ENDIF.
      ELSE.
        IF i_ignoring_case EQ 'X'.
          FIND PCRE i_regex IN c_in MATCH OFFSET e_offset IGNORING CASE.
        ELSE.
          FIND PCRE i_regex IN c_in MATCH OFFSET e_offset.
        ENDIF.
      ENDIF.

    ELSE.
      " REPLACE
      e_offset = 0.
      IF i_all_occurrences EQ 'X'.
        IF i_ignoring_case EQ 'X'.
          REPLACE ALL OCCURRENCES OF PCRE i_regex IN c_in WITH i_with IGNORING CASE.
        ELSE.
          REPLACE ALL OCCURRENCES OF PCRE i_regex IN c_in WITH i_with.
        ENDIF.
      ELSE.
        IF i_ignoring_case EQ 'X'.
          REPLACE FIRST OCCURRENCE OF PCRE i_regex IN c_in WITH i_with IGNORING CASE.
        ELSE.
          REPLACE FIRST OCCURRENCE OF PCRE i_regex IN c_in WITH i_with.
        ENDIF.
      ENDIF.

    ENDIF.

    e_subrc = sy-subrc.

  ENDMETHOD.
ENDCLASS.
