class ZCL_CA_LOGGER definition
  public
  final
  create private .

public section.
  type-pools ABAP .

  data HEADER type BAL_S_LOG read-only .
  data HANDLE type BALLOGHNDL read-only .
  data DB_NUMBER type BALOGNR read-only .

  class-methods NEW
    importing
      !OBJECT type CSEQUENCE optional
      !SUBOBJECT type CSEQUENCE optional
      !DESC type CSEQUENCE optional
      !CONTEXT type SIMPLE optional
    returning
      value(R_LOG) type ref to ZCL_CA_LOGGER .
  class-methods OPEN
    importing
      !OBJECT type CSEQUENCE
      !SUBOBJECT type CSEQUENCE
      !DESC type CSEQUENCE optional
      !LOGNUMBER type BALOGNR optional
      !CREATE_IF_DOES_NOT_EXIST type ABAP_BOOL default ABAP_FALSE
    returning
      value(R_LOG) type ref to ZCL_CA_LOGGER .
  methods A
    importing
      !OBJ_TO_LOG type ANY optional
      !CONTEXT type SIMPLE optional
      !CALLBACK_FORM type CSEQUENCE optional
      !CALLBACK_PROG type CSEQUENCE optional
      !CALLBACK_FM type CSEQUENCE optional
      !IMPORTANCE type BALPROBCL optional
    returning
      value(SELF) type ref to ZCL_CA_LOGGER .
  methods E
    importing
      !OBJ_TO_LOG type ANY optional
      !CONTEXT type SIMPLE optional
      !CALLBACK_FORM type CSEQUENCE optional
      !CALLBACK_PROG type CSEQUENCE optional
      !CALLBACK_FM type CSEQUENCE optional
      !IMPORTANCE type BALPROBCL optional
    returning
      value(SELF) type ref to ZCL_CA_LOGGER .
  methods EXPORT_TO_TABLE
    returning
      value(RT_BAPIRET) type BAPIRETTAB .
  methods FULLSCREEN .
  methods I
    importing
      !OBJ_TO_LOG type ANY optional
      !CONTEXT type SIMPLE optional
      !CALLBACK_FORM type CSEQUENCE optional
      !CALLBACK_PROG type CSEQUENCE optional
      !CALLBACK_FM type CSEQUENCE optional
      !IMPORTANCE type BALPROBCL optional
    returning
      value(SELF) type ref to ZCL_CA_LOGGER .
  methods POPUP .
  methods S
    importing
      !OBJ_TO_LOG type ANY optional
      !CONTEXT type SIMPLE optional
      !CALLBACK_FORM type CSEQUENCE optional
      !CALLBACK_PROG type CSEQUENCE optional
      !CALLBACK_FM type CSEQUENCE optional
      !IMPORTANCE type BALPROBCL optional
    returning
      value(SELF) type ref to ZCL_CA_LOGGER .
  methods W
    importing
      !OBJ_TO_LOG type ANY optional
      !CONTEXT type SIMPLE optional
      !CALLBACK_FORM type CSEQUENCE optional
      !CALLBACK_PROG type CSEQUENCE optional
      !CALLBACK_FM type CSEQUENCE optional
      !IMPORTANCE type BALPROBCL optional
    returning
      value(SELF) type ref to ZCL_CA_LOGGER .
  methods DISPLAY_LOG .
  methods ADD
    importing
      !OBJ_TO_LOG type ANY optional
      !CONTEXT type SIMPLE optional
      !CALLBACK_FORM type CSEQUENCE optional
      !CALLBACK_PROG type CSEQUENCE optional
      !CALLBACK_FM type CSEQUENCE optional
      !TYPE type SYMSGTY optional
      !IMPORTANCE type BALPROBCL optional
    returning
      value(SELF) type ref to ZCL_CA_LOGGER .
protected section.
private section.

  data AUTO_SAVE type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_CA_LOGGER IMPLEMENTATION.


  METHOD a.

    self = add( obj_to_log    = obj_to_log
    context       = context
    callback_form = callback_form
    callback_prog = callback_prog
    callback_fm   = callback_fm
    type          = 'A'
    importance    = importance ).
  ENDMETHOD.


  METHOD add.
    DATA: detailed_msg      TYPE bal_s_msg,
          free_text_msg     TYPE char200,
          msg_type          TYPE REF TO cl_abap_typedescr,
          msg_table_type    TYPE REF TO cl_abap_tabledescr,
          exception_data    TYPE bal_s_exc,
          log_numbers       TYPE bal_t_lgnm,
          log_handles       TYPE bal_t_logh,
          log_number        TYPE bal_s_lgnm,
          formatted_context TYPE bal_s_cont,
          formatted_params  TYPE bal_s_parm.

    FIELD-SYMBOLS: <table_of_messages> TYPE ANY TABLE,
                   <message_line>      TYPE any,
                   <bapi_msg>          TYPE bapiret2,
                   <bdc_msg>           TYPE bdcmsgcoll,
                   <context_val>       TYPE c.

    IF context IS NOT INITIAL.
      ASSIGN context TO <context_val> CASTING.
      formatted_context-value = <context_val>.
      formatted_context-tabname =
      cl_abap_typedescr=>describe_by_data( context )->get_ddic_header( )-tabname.
    ENDIF.

    IF callback_fm IS NOT INITIAL.
      formatted_params-callback-userexitf = callback_fm.
      formatted_params-callback-userexitp = callback_prog.
      formatted_params-callback-userexitt = 'F'.
    ELSEIF callback_form IS NOT INITIAL.
      formatted_params-callback-userexitf = callback_form.
      formatted_params-callback-userexitp = callback_prog.
      formatted_params-callback-userexitt = ' '.
    ENDIF.

    msg_type = cl_abap_typedescr=>describe_by_data( obj_to_log ).

    IF obj_to_log IS INITIAL.
      detailed_msg-msgty = sy-msgty.
      detailed_msg-msgid = sy-msgid.
      detailed_msg-msgno = sy-msgno.
      detailed_msg-msgv1 = sy-msgv1.
      detailed_msg-msgv2 = sy-msgv2.
      detailed_msg-msgv3 = sy-msgv3.
      detailed_msg-msgv4 = sy-msgv4.
    ELSEIF msg_type->type_kind = cl_abap_typedescr=>typekind_oref.
      exception_data-exception = obj_to_log.
      exception_data-msgty = type.
      exception_data-probclass = importance.
    ELSEIF msg_type->type_kind = cl_abap_typedescr=>typekind_table.
      ASSIGN obj_to_log TO <table_of_messages>.
      LOOP AT <table_of_messages> ASSIGNING <message_line>.
        add( obj_to_log = <message_line> ).
      ENDLOOP.
      RETURN.
    ELSEIF msg_type->absolute_name = '\TYPE=BAPIRET2'.
      ASSIGN obj_to_log TO <bapi_msg>.
      detailed_msg-msgty = <bapi_msg>-type.
      detailed_msg-msgid = <bapi_msg>-id.
      detailed_msg-msgno = <bapi_msg>-number.
      detailed_msg-msgv1 = <bapi_msg>-message_v1.
      detailed_msg-msgv2 = <bapi_msg>-message_v2.
      detailed_msg-msgv3 = <bapi_msg>-message_v3.
      detailed_msg-msgv4 = <bapi_msg>-message_v4.
    ELSEIF msg_type->absolute_name = '\TYPE=BDCMSGCOLL'.
      ASSIGN obj_to_log TO <bdc_msg>.
      detailed_msg-msgty = <bdc_msg>-msgtyp.
      detailed_msg-msgid = <bdc_msg>-msgid.
      detailed_msg-msgno = <bdc_msg>-msgnr.
      detailed_msg-msgv1 = <bdc_msg>-msgv1.
      detailed_msg-msgv2 = <bdc_msg>-msgv2.
      detailed_msg-msgv3 = <bdc_msg>-msgv3.
      detailed_msg-msgv4 = <bdc_msg>-msgv4.
    ELSE.
      free_text_msg = obj_to_log.
    ENDIF.

    IF free_text_msg IS NOT INITIAL.
      CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
        EXPORTING
          i_log_handle = me->handle
          i_msgty      = type
          i_probclass  = importance
          i_text       = free_text_msg
          i_s_context  = formatted_context
          i_s_params   = formatted_params.
    ELSEIF exception_data IS NOT INITIAL.
      CALL FUNCTION 'BAL_LOG_EXCEPTION_ADD'
        EXPORTING
          i_log_handle = me->handle
          i_s_exc      = exception_data.
    ELSEIF detailed_msg IS NOT INITIAL.
      detailed_msg-context = formatted_context.
      detailed_msg-params = formatted_params.
      detailed_msg-probclass = importance.
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = me->handle
          i_s_msg      = detailed_msg.
    ENDIF.

    IF auto_save = abap_true.
      APPEND me->handle TO log_handles.
      CALL FUNCTION 'BAL_DB_SAVE'
        EXPORTING
          i_t_log_handle       = log_handles
          i_2th_connection     = abap_true
          i_2th_connect_commit = abap_true
        IMPORTING
          e_new_lognumbers     = log_numbers.
      IF me->db_number IS INITIAL.
        READ TABLE log_numbers INDEX 1 INTO log_number.
        me->db_number = log_number-lognumber.
      ENDIF.
    ENDIF.

    self = me.

  ENDMETHOD.


  method DISPLAY_LOG.
  endmethod.


  method E.
  self = ADD(
  obj_to_log    = obj_to_log
  context       = context
  callback_form = callback_form
  callback_prog = callback_prog
  callback_fm   = callback_fm
  TYPE          = 'E'
  importance    = importance ).
  endmethod.


  method EXPORT_TO_TABLE.
  DATA: log_handle      TYPE bal_t_logh,
        message_handles TYPE bal_t_msgh,
        MESSAGE         TYPE bal_s_msg,
        bapiret2        TYPE bapiret2.

  FIELD-SYMBOLS <msg_handle> TYPE balmsghndl.

  INSERT handle INTO TABLE log_handle.

  CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
  EXPORTING
    i_t_log_handle = log_handle
  IMPORTING
    e_t_msg_handle = message_handles
  EXCEPTIONS
    msg_not_found  = 0.

  LOOP AT message_handles ASSIGNING <msg_handle>.
    CALL FUNCTION 'BAL_LOG_MSG_READ'
    EXPORTING
      i_s_msg_handle = <msg_handle>
    IMPORTING
      e_s_msg        = MESSAGE
    EXCEPTIONS
      OTHERS         = 3.
    IF sy-subrc IS INITIAL.
      MESSAGE ID MESSAGE-msgid
      TYPE MESSAGE-msgty
      NUMBER MESSAGE-msgno
      INTO bapiret2-MESSAGE
      WITH MESSAGE-msgv1 MESSAGE-msgv2 MESSAGE-msgv3 MESSAGE-msgv4.

      bapiret2-TYPE          = MESSAGE-msgty.
      bapiret2-ID            = MESSAGE-msgid.
      bapiret2-NUMBER        = MESSAGE-msgno.
      bapiret2-log_no        = <msg_handle>-log_handle. "last 2 chars missing!!
      bapiret2-log_msg_no    = <msg_handle>-msgnumber.
      bapiret2-message_v1    = MESSAGE-msgv1.
      bapiret2-message_v2    = MESSAGE-msgv2.
      bapiret2-message_v3    = MESSAGE-msgv3.
      bapiret2-message_v4    = MESSAGE-msgv4.
      bapiret2-system        = sy-sysid.
      APPEND bapiret2 TO rt_bapiret.
    ENDIF.
  ENDLOOP.

  endmethod.


  method FULLSCREEN.

  DATA:
        profile        TYPE bal_s_prof,
        lt_log_handles TYPE bal_t_logh.

  APPEND me->handle TO lt_log_handles.

  CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
  IMPORTING
    e_s_display_profile = profile.

  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
  EXPORTING
    i_s_display_profile = profile
*       i_t_log_handle      = me->handle
    i_t_log_handle      = lt_log_handles.

  endmethod.


  method I.
  self = ADD(
  obj_to_log    = obj_to_log
  context       = context
  callback_form = callback_form
  callback_prog = callback_prog
  callback_fm   = callback_fm
  TYPE          = 'I'
  importance    = importance ).
  endmethod.


  method NEW.

  FIELD-SYMBOLS <context_val> TYPE C.

  CREATE OBJECT r_log.
  r_log->header-object = object.
  r_log->header-subobject = subobject.
  r_log->header-extnumber = desc.
  IF object IS NOT INITIAL AND subobject IS NOT INITIAL.
    r_log->auto_save = abap_true.
  ENDIF.

  IF context IS SUPPLIED.
    r_log->header-context-tabname =
    cl_abap_typedescr=>describe_by_data( context )->get_ddic_header( )-tabname.
    ASSIGN context TO <context_val> CASTING.
    r_log->header-context-VALUE = <context_val>.
  ENDIF.

  CALL FUNCTION 'BAL_LOG_CREATE'
  EXPORTING
    i_s_log      = r_log->header
  IMPORTING
    e_log_handle = r_log->handle.

* BAL_LOG_CREATE will fill in some additional header data.
* This FM updates our instance attribute to reflect that.
  CALL FUNCTION 'BAL_LOG_HDR_READ'
  EXPORTING
    i_log_handle = r_log->handle
  IMPORTING
    e_s_log      = r_log->header.

  endmethod.


  METHOD open.

    DATA:
      filter             TYPE bal_s_lfil,
      desc_filter        TYPE bal_s_extn,
      obj_filter         TYPE bal_s_obj,
      subobj_filter      TYPE bal_s_sub,
      log_filter         TYPE bal_s_logn,

      found_headers      TYPE balhdr_t,
      most_recent_header TYPE balhdr,
      handles_loaded     TYPE bal_t_logh.

    subobj_filter-option = obj_filter-option = 'EQ'.
    subobj_filter-sign   = obj_filter-sign   = 'I'.

    obj_filter-low = object.
    APPEND obj_filter TO filter-object.

    subobj_filter-low = subobject.
    APPEND subobj_filter TO filter-subobject.

    IF desc IS SUPPLIED.
      desc_filter-option = 'EQ'.
      desc_filter-sign   = 'I'.
      desc_filter-low = desc.
      APPEND desc_filter TO filter-extnumber.
    ENDIF.

    IF lognumber IS SUPPLIED.
      log_filter-option = 'EQ'.
      log_filter-sign = 'I'.
      log_filter-low = lognumber.
      APPEND log_filter TO filter-lognumber.
    ENDIF.

    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        i_s_log_filter = filter
      IMPORTING
        e_t_log_header = found_headers
      EXCEPTIONS
        log_not_found  = 1.

    IF sy-subrc = 1.
      IF create_if_does_not_exist = abap_true.
        r_log = zcl_ca_logger=>new( object    = object
        subobject = subobject
        desc      = desc ).
      ENDIF.
      RETURN.
    ENDIF.

* Delete all but the last row.  Keep the found_headers table this way
* so we can pass it to BAL_DB_LOAD.
    IF lines( found_headers ) > 1.
      DELETE found_headers TO ( lines( found_headers ) - 1 ).
    ENDIF.

    READ TABLE found_headers INDEX 1 INTO most_recent_header.

    CREATE OBJECT r_log.

    r_log->auto_save = abap_true.
    r_log->db_number = most_recent_header-lognumber.
    r_log->handle = most_recent_header-log_handle.

    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header = found_headers.

    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING
        i_log_handle = r_log->handle
      IMPORTING
        e_s_log      = r_log->header.

  ENDMETHOD.


  method POPUP.
  DATA profile TYPE bal_s_prof.

  CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
  IMPORTING
    e_s_display_profile = profile.

  DATA lt_handle TYPE bal_t_logh.

  APPEND me->handle TO lt_handle.

  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
  EXPORTING
    i_s_display_profile = profile
    i_t_log_handle      = lt_handle.

  endmethod.


  method S.
  self = ADD(
  obj_to_log    = obj_to_log
  context       = context
  callback_form = callback_form
  callback_prog = callback_prog
  callback_fm   = callback_fm
  TYPE          = 'S'
  importance    = importance ).
  endmethod.


  method W.
  self = ADD(
  obj_to_log    = obj_to_log
  context       = context
  callback_form = callback_form
  callback_prog = callback_prog
  callback_fm   = callback_fm
  TYPE          = 'W'
  importance    = importance ).
  endmethod.
ENDCLASS.
