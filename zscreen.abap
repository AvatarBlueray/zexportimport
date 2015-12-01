*&---------------------------------------------------------------------*
*& Report  ZSCREEN                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&  create alv grid defenition and implementation                                                                    *
*&---------------------------------------------------------------------*



REPORT  zscreen                                         .
TYPE-POOLS
  : slis
  .
TABLES
  : sbpt_cmnds
  .

SELECTION-SCREEN PUSHBUTTON 49(30) text-001 USER-COMMAND btn.

PARAMETERS
    : p_dynnr TYPE sy-dynnr DEFAULT '0100'.
.


SELECT-OPTIONS
    :  s_seo FOR sbpt_cmnds-dyn_meth NO INTERVALS
    .



TYPES
    : BEGIN OF t_dyn_meth
    ,    dyn_meth TYPE sbpt_cmnds-dyn_meth
    ,    checkbox
    , END OF t_dyn_meth
    .

DATA
      : docking_left TYPE REF TO cl_gui_docking_container
      , text_editor_top TYPE REF TO cl_gui_textedit
      , lt_source TYPE TABLE OF tline-tdline
      , ls_source TYPE tline-tdline
      , lv_tmp_dynn(4) TYPE n
      , lv_str TYPE string
      , lv_sconame  TYPE seosconame
      , lv_editorder  TYPE seoorder
      , lt_dyn_meth TYPE TABLE OF t_dyn_meth
      .


DEFINE add_line.
  ls_source = &1.

  replace all occurrences of '$!$' in ls_source with p_dynnr.
  append  ls_source to lt_source.

END-OF-DEFINITION.

AT SELECTION-SCREEN.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_seo-low .
  PERFORM populate_s_seo.

AT SELECTION-SCREEN OUTPUT.
  REFRESH lt_source.

  lv_tmp_dynn = p_dynnr.
  p_dynnr = lv_tmp_dynn.

  REFRESH lt_source.
  IF s_seo[] IS NOT INITIAL .
    add_line '*----------------------------'.
    add_line '*class defenition'.
    add_line 'CLASS lcl_event_receiver DEFINITION deferred.'.
  ENDIF.

  add_line ' '.
  add_line ' '.
  add_line '*----------------------------'.
  add_line '*-- top screen $!$'.
  IF s_seo[] IS NOT INITIAL .
    add_line '      , g_event_receiver  TYPE REF TO lcl_event_receiver'.
    add_line ' '.
  ENDIF.
  add_line '      , ok_code      TYPE sy-ucomm'.
  add_line '      , gr_alv_$!$            TYPE REF TO cl_gui_alv_grid'.
  add_line '      , gr_cont_$!$           TYPE REF TO cl_gui_custom_container'.
  add_line '      , gt_fieldcat_$!$       TYPE lvc_t_fcat'.
  add_line '      , gt_sort_$!$           TYPE lvc_t_sort'.
  add_line '      , gs_layout_$!$         TYPE lvc_s_layo'.
  add_line '      , gs_vari_$!$           TYPE disvariant  '.
  add_line ' '.
  add_line ' '.

  IF s_seo[] IS NOT INITIAL .
    add_line '*----------------------------'.
    add_line '*class defenition  '.
    add_line ' '.
    add_line ' '.
    add_line ' '.
    add_line 'CLASS lcl_event_receiver DEFINITION.'.
    add_line ' '.
    add_line ' '.
    add_line '  PUBLIC SECTION.'.
    add_line ' '.
    add_line '     METHODS'.
    add_line ' '.
    LOOP AT s_seo.
      TRANSLATE s_seo-low TO UPPER CASE.
      CONCATENATE  '        ,   handle_' s_seo-low '_$!$' INTO lv_str.
      add_line lv_str.
      CONCATENATE '                FOR EVENT ' s_seo-low ' OF CL_GUI_ALV_GRID' INTO lv_str SEPARATED BY space.
      add_line lv_str.
      SELECT COUNT(  *  )
        FROM seosubcodf
        WHERE clsname = 'CL_GUI_ALV_GRID'
        AND   cmpname = s_seo-low.
      IF sy-subrc = 0.
        lv_str = '                              IMPORTING '.
        SELECT  sconame editorder INTO (lv_sconame, lv_editorder)
          FROM seosubcodf
          WHERE clsname = 'CL_GUI_ALV_GRID'
          AND   cmpname = s_seo-low
          ORDER BY editorder ASCENDING
          .

          CONCATENATE lv_str lv_sconame INTO lv_str SEPARATED BY space.
        ENDSELECT.

        add_line lv_str.

      ENDIF.
    ENDLOOP.
    add_line ' '.
    add_line '.'.
    add_line ' '.
    add_line 'ENDCLASS. "lcl_event_receiver DEFINITION'.
    add_line ' '.
    add_line ' '.
    add_line ' '.
    add_line 'CLASS lcl_event_receiver IMPLEMENTATION.'.
    add_line ' '.
    add_line ' '.
    LOOP AT s_seo.
      CONCATENATE '  METHOD handle_' s_seo-low '_$!$.' INTO lv_str.
      add_line lv_str.
      add_line ' '.
      add_line '  ENDMETHOD.'.
    ENDLOOP.

    add_line ' '.
    add_line ' '.
    add_line 'ENDCLASS. '.
    add_line ' '.

    add_line ' '.
    add_line '*----------------------------'.
    add_line '*initialization'.
    add_line ' initialization.'.
    add_line ' create object g_event_receiver .'.

  ENDIF.

  add_line ' '.
  add_line '*----------------------------'.
  add_line '*screen logic'.
  add_line ' '.
  add_line 'PROCESS BEFORE OUTPUT.'.
  add_line ' MODULE STATUS_$!$.'.
  add_line '*'.
  add_line 'PROCESS AFTER INPUT.'.
  add_line ' MODULE USER_COMMAND_$!$.'.
  add_line ' '.
  add_line ' '.

  add_line '*----------------------------'.
  add_line '*modules'.

  add_line 'MODULE status_$!$ OUTPUT.'.
  add_line '  SET PF-STATUS ''STATUS$!$''.'.
  add_line '  SET TITLEBAR ''TITLEBAR$!$''.'.
  add_line ''.
  add_line ''.
  add_line '  IF gr_alv_$!$ IS NOT BOUND.'.
  add_line '    CREATE OBJECT gr_cont_$!$'.
  add_line '      EXPORTING'.
  add_line '        container_name = ''CONT_$!$''.'.
  add_line ''.
  add_line '    CREATE OBJECT gr_alv_$!$'.
  add_line '      EXPORTING'.
  add_line '        i_parent = gr_cont_$!$.'.
  add_line ''.
  add_line '    gs_layout_$!$-no_rowmove = ''X''.'.
  add_line '    gs_layout_$!$-cwidth_opt = ''X''.'.
  add_line ''.
  add_line '    gs_layout_$!$-zebra = ''X''.'.
  add_line '    gs_layout_$!$-no_rowmark = ''''.'.
  add_line '    gs_layout_$!$-sel_mode = ''A''.'.
  add_line ''.
  add_line '    gs_vari_$!$-report = sy-repid.'.
  add_line '    gs_vari_$!$-handle = ''$!$''.'.
  add_line ''.
  add_line ''.
  add_line '    CALL FUNCTION ''LVC_FIELDCATALOG_MERGE'''.
  add_line '      EXPORTING'.
  add_line '        i_prog_name      = ''Z_CARDACC_REPORT_ST'''.
  add_line '        i_dynnn          = ''$!$'''.
  add_line '        i_structure_name = ''Z_CARDACC_REPORT_STR_$!$'''.
  add_line '      CHANGING'.
  add_line '        ct_fieldcat      = gt_fieldcat_$!$.'.
  add_line ''.
  IF s_seo[] IS NOT INITIAL.
    LOOP AT s_seo.
      CONCATENATE 'SET HANDLER g_event_receiver->handle_' s_seo-low '_$!$ FOR gr_alv_$!$.' INTO lv_str.
      add_line lv_str.
      add_line ' '.
    ENDLOOP.
  ENDIF.
  add_line ''.
  add_line '    CALL METHOD gr_alv_$!$->set_table_for_first_display'.
  add_line '      EXPORTING'.
  add_line '        i_save          = ''A'''.
  add_line '        is_layout       = gs_layout_$!$'.
  add_line '        is_variant      = gs_vari_$!$'.
  add_line '      CHANGING'.
  add_line '        it_sort         = gt_sort_$!$'.
  add_line '        it_outtab       = gt_$!$'.
  add_line '        it_fieldcatalog = gt_fieldcat_$!$.'.
  add_line ''.
  add_line '  ELSE.'.
  add_line ''.
  add_line '    CALL METHOD gr_alv_$!$->get_frontend_layout'.
  add_line '      IMPORTING'.
  add_line '        es_layout = gs_layout_$!$.'.
  add_line ''.
  add_line '    gs_layout_$!$-cwidth_opt = ''X''.'.
  add_line ''.
  add_line '    CALL METHOD gr_alv_$!$->set_frontend_layout'.
  add_line '      EXPORTING'.
  add_line '        is_layout = gs_layout_$!$.'.
  add_line ''.
  add_line ''.
  add_line '    gr_alv_$!$->refresh_table_display( ).'.
  add_line '  ENDIF.'.
  add_line ''.
  add_line 'ENDMODULE. '.
  add_line ''.
  add_line ''.
  add_line ''.
  add_line ''.
  add_line 'MODULE user_command_$!$ INPUT.'.
  add_line ''.
  add_line '  CASE  ok_code .'.
  add_line '    WHEN ''BACK''.'.
  add_line '      SET SCREEN 0.'.
  add_line '*   when ''XXXXXXX''.'.
  add_line ''.
  add_line '  ENDCASE.'.
  add_line ''.
  add_line 'ENDMODULE.    '.


  IF docking_left IS INITIAL.
    CREATE OBJECT:

    docking_left
    EXPORTING repid = sy-repid
    dynnr = sy-dynnr
    side = docking_left->dock_at_bottom
    extension = 400.

    CREATE OBJECT text_editor_top
         EXPORTING
           parent = docking_left.

  ENDIF.

  CALL METHOD text_editor_top->set_text_as_r3table
    EXPORTING
      table  = lt_source
    EXCEPTIONS
      OTHERS = 1.

*&---------------------------------------------------------------------*
*&      Form  populate_s_seo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*


FORM populate_s_seo.



  DATA
       : ls_fieldcat TYPE slis_fieldcat_alv
       , ls_private  TYPE slis_data_caller_exit
       , lt_fieldcat TYPE slis_t_fieldcat_alv
       , l_exit
       .

  FIELD-SYMBOLS
                 : <fs_dyn_meth> TYPE t_dyn_meth
                 .


  DEFINE add_dyn_meth.
    append initial line to lt_dyn_meth  assigning <fs_dyn_meth>.
    <fs_dyn_meth>-dyn_meth = &1.
  END-OF-DEFINITION.

  REFRESH lt_dyn_meth.

  SELECT DISTINCT cmpname  AS dyn_meth INTO TABLE lt_dyn_meth
    FROM seosubco
    WHERE clsname = 'CL_GUI_ALV_GRID'
    AND  cmptype = 2
    .




  ls_fieldcat-fieldname = 'DYN_METH' .
  ls_fieldcat-inttype   = 'C' .
  ls_fieldcat-outputlen = '61' .
  "ls_fieldcat-coltext   = 'fieldname' .
  ls_fieldcat-seltext_l    = 'Событие' .
  APPEND ls_fieldcat TO lt_fieldcat.
  ls_private-columnopt = 'X'.

  IF s_seo[] IS NOT INITIAL.


    LOOP AT lt_dyn_meth ASSIGNING  <fs_dyn_meth> WHERE dyn_meth IN s_seo.
      <fs_dyn_meth>-checkbox = 'X'.
    ENDLOOP.

  ENDIF.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_screen_start_column = 20
      i_selection           = 'X'
      i_zebra               = 'X'
      it_fieldcat           = lt_fieldcat
      i_tabname             = 'LT_DYN_METH'
      i_checkbox_fieldname  = 'CHECKBOX'
      is_private            = ls_private
    IMPORTING
      e_exit                = l_exit
    TABLES
      t_outtab              = lt_dyn_meth[].

  CHECK l_exit = space.

  REFRESH s_seo.

  s_seo-sign = 'I'.
  s_seo-option = 'EQ' .

  LOOP AT lt_dyn_meth ASSIGNING <fs_dyn_meth> WHERE checkbox = 'X'.
    s_seo-low = <fs_dyn_meth>-dyn_meth.
    APPEND s_seo.

  ENDLOOP.
  READ TABLE s_seo INDEX 1.



ENDFORM.                    "populate_s_seo
