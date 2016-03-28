*&---------------------------------------------------------------------*
*& Report  Z_DEEP_SEARCH
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z_deep_search.

PARAMETERS
  : p_prog TYPE sy-cprog OBLIGATORY DEFAULT 'ZAM_PR_CARDACC_REPORT'
  , p_search TYPE sy-cprog OBLIGATORY DEFAULT 'bca_paymitem'
  .


TYPES
  : BEGIN OF t_func
  ,  name TYPE  rs38l_fnam
  ,  exist TYPE c
  , END OF t_func

  , t_code TYPE c LENGTH 200
  , tt_code TYPE TABLE OF t_code

  , tt_func TYPE STANDARD TABLE OF t_func
  .



DATA
      : treename  LIKE  dirtree-tname
      , lt_result TYPE  match_result_tab
      , lt_functions TYPE tt_func
      , gt_func TYPE TABLE OF t_func
      , moff TYPE i
      , mlen TYPE i
      , itab TYPE tt_code



      .


DATA nodetab LIKE STANDARD TABLE OF  snodetext WITH HEADER LINE.

FIELD-SYMBOLS
               : <fs1_node>  TYPE snodetext
               , <fs1_result> TYPE match_result
               , <fs1_tab> TYPE  t_code
               , <fs1_func> TYPE t_func

               .



CONCATENATE 'PG_' p_prog INTO treename.



CALL FUNCTION 'WB_TREE_SELECT'
  EXPORTING
    treename            = treename
    ignore_current_tree = 'X'
  TABLES
    nodetab             = nodetab[]
  EXCEPTIONS
    not_found           = 1.


DELETE nodetab WHERE NOT type EQ 'OI'.

LOOP AT nodetab ASSIGNING <fs1_node>.

  REFRESH itab.

  READ REPORT <fs1_node>-name INTO itab.

  FIND ALL OCCURRENCES OF  p_search
  IN TABLE itab IGNORING CASE
  RESULTS  lt_result.

  IF lt_result[] IS NOT INITIAL.
    WRITE : / '============', / <fs1_node>-name, / '============'.

    LOOP AT lt_result ASSIGNING <fs1_result>.

      READ TABLE itab ASSIGNING <fs1_tab> INDEX <fs1_result>-line.

      PERFORM check_coment USING  <fs1_tab>.

      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.
      WRITE : / 'Line:', <fs1_result>-line, ':'.

      READ TABLE itab ASSIGNING <fs1_tab> INDEX <fs1_result>-line.
      WRITE : <fs1_tab>.

    ENDLOOP.

  ENDIF.

ENDLOOP.



LOOP AT nodetab ASSIGNING <fs1_node>.

  REFRESH itab.

  READ REPORT <fs1_node>-name INTO itab.


  PERFORM check_call USING <fs1_node>-name  itab lt_functions.

ENDLOOP.


ULINE.
ULINE.
ULINE.

LOOP AT lt_functions ASSIGNING <fs1_func>.

  CHECK <fs1_func>-exist IS NOT INITIAL.

  WRITE : /  <fs1_func>-name.

ENDLOOP.

*&---------------------------------------------------------------------*
*&      Form  check_call
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FNAME      text
*----------------------------------------------------------------------*
FORM check_call USING fname
                    source TYPE tt_code
                    p_lt_functions TYPE tt_func.

  DATA
        : lt_result TYPE  match_result_tab
        , itab TYPE tt_code
        , lv_fname TYPE rs38l_fnam
        , lv_exist TYPE c

        , moff TYPE i
        , mlen TYPE i
        .


  FIELD-SYMBOLS
                 : <fs_result> TYPE match_result
                 , <fs_tab> TYPE  t_code
                 , <fs_func> TYPE t_func
                 .

  itab = source.



  FIND ALL OCCURRENCES OF  REGEX 'call\s+function\s+''(z\S+)'''
  IN TABLE itab IGNORING CASE
  RESULTS  lt_result.

  IF lt_result[] IS NOT INITIAL.
*    WRITE : / '============', / fname, / '============'.

    LOOP AT lt_result ASSIGNING <fs_result>.
      READ TABLE itab ASSIGNING <fs_tab> INDEX <fs_result>-line.

      PERFORM check_coment USING  <fs_tab>.

      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      FIND REGEX 'z[a-z_0-9]+' IN <fs_tab>
        IGNORING CASE
        MATCH OFFSET moff
        MATCH LENGTH mlen .

      lv_fname =  <fs_tab>+moff(mlen).
      TRANSLATE lv_fname USING ''' '.
      CONDENSE lv_fname.

      PERFORM check_fname   USING fname lv_fname  p_lt_functions CHANGING lv_exist.

      IF lv_exist IS INITIAL.
        CONTINUE.
      ENDIF.

      WRITE :/ fname, '->', lv_fname.

      WRITE : / 'Line:', <fs_result>-line, ':'.


      WRITE : <fs_tab>.

    ENDLOOP.

  ENDIF.
ENDFORM.                    "check_call
*&---------------------------------------------------------------------*
*&      Form  CHECK_FNAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_FUNCTIONS  text
*      -->P_LV_FNAME  text
*      <--P_LV_EXIST  text
*----------------------------------------------------------------------*
FORM check_fname
                             "Вставьте правильное имя для <...>.
                  USING    p_source TYPE  rs38l_fnam
                           p_lv_fname TYPE  rs38l_fnam
                           p_lt_functions TYPE tt_func
                  CHANGING p_lv_exist.


  FIELD-SYMBOLS
                 : <fs_func> TYPE t_func
                 , <fs_result> TYPE match_result
                 , <fs_tab> TYPE  t_code
                 .

  DATA
        : lt_source TYPE tt_code
        , lt_result TYPE  match_result_tab
        .

  CLEAR p_lv_exist.

  READ TABLE p_lt_functions WITH KEY name = p_lv_fname ASSIGNING <fs_func>.

  IF sy-subrc = 0.
    p_lv_exist = <fs_func>-exist.
  ELSE.
    APPEND INITIAL LINE TO p_lt_functions ASSIGNING <fs_func>.
    <fs_func>-name = p_lv_fname.

    PERFORM get_fname_source
                USING
                   p_lv_fname
                   lt_source.

    FIND ALL OCCURRENCES OF  p_search
    IN TABLE lt_source IGNORING CASE
    RESULTS  lt_result.


    IF lt_result[] IS NOT INITIAL.

      LOOP AT lt_result ASSIGNING <fs_result>.


        READ TABLE lt_source ASSIGNING <fs_tab> INDEX <fs_result>-line.
        PERFORM check_coment USING  <fs_tab>.

        IF sy-subrc = 0.
          CONTINUE.
        ENDIF.

        <fs_func>-exist = 'X'.

      ENDLOOP.



      IF <fs_func>-exist = 'X'..
        WRITE : / '============', / p_source , '->',  p_lv_fname ,  / '============'.

        LOOP AT lt_result ASSIGNING <fs_result>.

          READ TABLE lt_source ASSIGNING <fs_tab> INDEX <fs_result>-line.

          PERFORM check_coment USING  <fs_tab>.

          IF sy-subrc = 0.
            CONTINUE.
          ENDIF.

          WRITE : / 'Line:', <fs_result>-line, ':'.


          WRITE : <fs_tab>.

        ENDLOOP.
      ENDIF.



    ENDIF.

    PERFORM check_call USING  p_lv_fname lt_source
                             p_lt_functions .

  ENDIF.

ENDFORM.                    " CHECK_FNAME


*&---------------------------------------------------------------------*
*&      Form  get_fname_source
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FNAME      text
*      -->SOURCE     text
*----------------------------------------------------------------------*
FORM get_fname_source USING fname TYPE rs38l_fnam
                            source TYPE tt_code.

  REFRESH source.

  DATA
        :  gv_prog_name    TYPE trdir-name
        , lv_tfdir TYPE tfdir
        .

  SELECT SINGLE *
    INTO lv_tfdir
    FROM   tfdir
    WHERE  funcname = fname.
  IF sy-subrc EQ 0.
    CONCATENATE lv_tfdir-pname 'U' lv_tfdir-include INTO gv_prog_name.
    gv_prog_name = gv_prog_name+3(30).

  ELSE.
    EXIT.
  ENDIF.

* read the report code lines in an internal table
  READ REPORT gv_prog_name INTO source.

ENDFORM.                    "get_fname_source


*&---------------------------------------------------------------------*
*&      Form  check_coment
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_STRING   text
*----------------------------------------------------------------------*
FORM check_coment USING p_string.
  FIND REGEX '^\s*\*' IN p_string.

  CHECK sy-subrc NE 0.

  FIND 'type' IN p_string
  IGNORING CASE .
  CHECK sy-subrc NE 0.

  FIND 'like' IN p_string
  IGNORING CASE .


ENDFORM.                    "check_coment
