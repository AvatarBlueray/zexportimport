
REPORT z_exportimport.

TABLES dd03l.


PARAMETERS: p_dir(80) DEFAULT 'D:\EXP\'. " default dir to hold data

SELECT-OPTIONS
 : s_table FOR dd03l-tabname NO INTERVALS " table list to export import
 .

PARAMETERS
  : p_exp TYPE checkbox DEFAULT 'X'   " default data exported
  , p_del TYPE checkbox               " purge  table before import
  .


DATA
      :    filename TYPE string
      ,    up_to_limit TYPE i
      ,    dref        TYPE REF TO data

      ,      ifc TYPE lvc_t_fcat
      ,      lv_count TYPE i
      ,      lv_debug(1) TYPE c VALUE ''
      ,      lt_keyfields TYPE TABLE OF   dd03p
      ,      lv_key_count TYPE i

      ,      lv_tabname TYPE tabname16
      ,      lt_where TYPE TABLE OF string
      ,      ls_where TYPE string

      ,      lv_lines TYPE i
      ,      lv_index TYPE sy-index

      ,      lv_open_bracket TYPE c
      ,      lv_close_bracket TYPE c
      ,      lv_eq_gt TYPE string
      ,      lv_and_or TYPE string
      ,      lv_tmp_str TYPE string

      ,      line_length TYPE i
      ,      lt_files TYPE TABLE OF file_info
      ,      lv_file_count TYPE i

      ,      lv_filter TYPE string
      ,      lv_directory TYPE string
      ,      lv_file_name TYPE string
*      ,      lt_allow_table TYPE TABLE OF tabname

      .


FIELD-SYMBOLS
               : <fs_keyfields>  TYPE dd03p
               , <fs_field> TYPE ANY
               , <fs_fcat> TYPE lvc_s_fcat
               , <fs_file> TYPE file_info
               , <it_outtab> TYPE STANDARD TABLE
               , <wa_outtab> TYPE ANY

               .

AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'I_CNOW_IT_IS_DANGEROUS'. " magic keyword must be submit on selection screen (where transaction name input)
      " that confirm you understand you overwrite table in db
      lv_debug ='X'.
      MESSAGE 'GOD mode is on :)' TYPE 'S'.
  ENDCASE.

START-OF-SELECTION.


*  APPEND 'ZTAB_NAME' TO lt_allow_table.
 


  filename = p_dir.

  LOOP AT s_table.
    CLEAR
    : line_length
    , lv_count
    .

*    READ TABLE lt_allow_table TRANSPORTING NO FIELDS WITH KEY table_line = s_table-low.
*    IF sy-subrc NE 0.
*      CONTINUE.
*    ENDIF.


    IF  lv_debug IS  INITIAL AND p_exp IS INITIAL .
      CONTINUE.
    ENDIF.


    SELECT COUNT( * )
      FROM dd03l
      INTO lv_count
      WHERE tabname = s_table-low.

    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    REFRESH ifc.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = s_table-low
      CHANGING
        ct_fieldcat      = ifc[].
    IF sy-subrc <> 0.

    ENDIF.

    LOOP AT ifc ASSIGNING <fs_fcat>.
      ADD <fs_fcat>-dd_outlen TO line_length.
      ADD 1 TO line_length.
    ENDLOOP.

    up_to_limit = 150 * 1024 * 1024 / line_length.

    lv_tabname = s_table-low.


    REFRESH lt_keyfields.

    CALL FUNCTION 'CUTA_DD_TABLE_KEYFIELDS_GET'
      EXPORTING
        dbtab_name = lv_tabname
      TABLES
        keyfields  = lt_keyfields.

    DELETE lt_keyfields  WHERE fieldname EQ '.INCLUDE'.

    DESCRIBE TABLE lt_keyfields LINES lv_key_count.

    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog = ifc
      IMPORTING
        ep_table        = dref.
    ASSIGN dref->* TO <it_outtab>.

    CREATE DATA dref LIKE LINE OF <it_outtab>.
    ASSIGN dref->* TO <wa_outtab>.

    IF p_exp IS NOT INITIAL.
      CLEAR lv_count.
      DO .
        REFRESH lt_where.

        DO lv_key_count TIMES.
          lv_index = sy-index.

          LOOP AT lt_keyfields ASSIGNING <fs_keyfields>.
            IF sy-tabix > lv_index.
              EXIT.
            ENDIF.

            IF sy-tabix = 1.
              lv_open_bracket = '('.
            ELSE.
              CLEAR lv_open_bracket.
            ENDIF.

            IF sy-tabix = lv_index.
              lv_close_bracket = ')'.
              lv_eq_gt = 'GT'.
            ELSE.
              CLEAR lv_close_bracket.
              lv_eq_gt = 'EQ'.
            ENDIF.

            IF lv_index > 1.
              IF sy-tabix > 1.
                lv_and_or = 'AND'.
              ELSE.
                lv_and_or = 'OR'.

              ENDIF.
            ELSE.
              CLEAR lv_and_or.
            ENDIF.
            ASSIGN COMPONENT <fs_keyfields>-fieldname OF STRUCTURE <wa_outtab> TO <fs_field>.
            lv_tmp_str = <fs_field>.

            CONCATENATE '''' lv_tmp_str '''' INTO lv_tmp_str.

            CONCATENATE lv_and_or lv_open_bracket
                    <fs_keyfields>-fieldname
                 lv_eq_gt lv_tmp_str lv_close_bracket

                 INTO ls_where SEPARATED BY space.

            APPEND ls_where TO lt_where.

          ENDLOOP.

        ENDDO.



        SELECT * FROM (s_table-low) INTO  TABLE <it_outtab>
          UP TO up_to_limit ROWS
          WHERE (lt_where)
          ORDER BY PRIMARY KEY.
        IF sy-subrc NE 0.
          EXIT.
        ENDIF.

        DESCRIBE TABLE <it_outtab> LINES lv_lines.
        READ TABLE <it_outtab> INTO <wa_outtab> INDEX lv_lines.


        ADD 1 TO lv_count.

        PERFORM wrap_download USING <it_outtab> s_table-low lv_count.
        REFRESH <it_outtab>.

      ENDDO.

    ELSE.

      IF p_del IS NOT INITIAL.
        DELETE FROM (s_table-low).
      ENDIF.

      CONCATENATE s_table-low '*.dat' INTO lv_filter.
      lv_directory = p_dir.

      CALL METHOD cl_gui_frontend_services=>directory_list_files
        EXPORTING
          directory                   = lv_directory
          filter                      = lv_filter
          files_only                  = 'X'
        CHANGING
          file_table                  = lt_files
          count                       = lv_file_count
        EXCEPTIONS
          cntl_error                  = 1
          directory_list_files_failed = 2
          wrong_parameter             = 3
          error_no_gui                = 4
          not_supported_by_gui        = 5
          OTHERS                      = 6.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.


      LOOP AT lt_files ASSIGNING <fs_file>.
        lv_file_name = <fs_file>-filename.
        PERFORM wrap_upload USING lv_file_name  CHANGING <it_outtab>.
        INSERT  (s_table-low) FROM TABLE <it_outtab> ACCEPTING DUPLICATE KEYS .

      ENDLOOP.


      MESSAGE 'Data imported.' TYPE 'S'.

    ENDIF.


  ENDLOOP.
*&---------------------------------------------------------------------*
*&      Form  wrap_download
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_TABLE   text
*----------------------------------------------------------------------*
FORM wrap_download USING it_table TYPE STANDARD TABLE
                                  tab_name TYPE tabname
                                  count TYPE i.


  DATA
        : lv_n(3) TYPE n
        , filename TYPE string
        .
  lv_n = count.

  CONCATENATE  p_dir '\' tab_name lv_n '.dat' INTO filename.
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = filename
      write_field_separator   = 'X'
      dat_mode                = 'X'
      trunc_trailing_blanks   = 'X'
      confirm_overwrite       = 'X'
    TABLES
      data_tab                = it_table
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "wrap_download

*&---------------------------------------------------------------------*
*&      Form  wrap_upload
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_TABLE   text
*----------------------------------------------------------------------*
FORM wrap_upload USING p_file_name TYPE string
                 CHANGING it_table TYPE STANDARD TABLE.

  DATA
        :filename TYPE string
        .

  CONCATENATE p_dir '\' p_file_name INTO filename.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = filename
      has_field_separator     = 'X'
      dat_mode                = 'X'
    TABLES
      data_tab                = it_table
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    "wrap_upload
