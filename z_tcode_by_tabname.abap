*&---------------------------------------------------------------------*
*& Report  ZTCODE_BY_TABNAME                                        *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ztcode_by_tabname                                        .


PARAMETERS
  : tabname TYPE tabname OBLIGATORY
  .

DATA
      : lv_tcode TYPE tcode
      , lt_dd26s TYPE TABLE OF dd26s
      , lv_vclname  TYPE vcl_name
      .

FIELD-SYMBOLS
               : <fs_dd26s> TYPE dd26s
               .

START-OF-SELECTION.

  WRITE : / 'Ведение таблиц'.
  ULINE.

  PERFORM get_tcode_by_tabname USING tabname.

  write : /' '.
  write : /' '.
  write : /' '.

  ULINE.
  WRITE : / 'Ведение  ракурсов'.
  ULINE.
  SELECT DISTINCT viewname  AS viewname
    INTO CORRESPONDING FIELDS OF TABLE lt_dd26s
    FROM dd26s
    WHERE tabname = tabname
    .


  LOOP AT lt_dd26s ASSIGNING <fs_dd26s>.
    WRITE : / 'ракурс ', <fs_dd26s>-viewname.
    PERFORM get_tcode_by_tabname USING <fs_dd26s>-viewname.
  ENDLOOP.

  write : /' '.
  write : /' '.
  write : /' '.
  ULINE.
  WRITE : / 'Ведение  кластеров ракурсов'.
  ULINE.

  SELECT vclname INTO lv_vclname
    FROM vclstruc
    WHERE object  = tabname.

    WRITE : / 'кластер ракурсов ', lv_vclname.
    PERFORM get_tcode_by_tabname USING lv_vclname.
  ENDSELECT.

  LOOP AT lt_dd26s ASSIGNING <fs_dd26s>.
    SELECT vclname INTO lv_vclname
      FROM vclstruc
      WHERE object  = <fs_dd26s>-viewname.

      WRITE : / 'кластер ракурсов ', lv_vclname.
      PERFORM get_tcode_by_tabname USING lv_vclname.
    ENDSELECT.
  ENDLOOP.
  ULINE.

*&---------------------------------------------------------------------*
*&      Form  get_tcode_by_tabname
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TABNAME    text
*      -->TCODE      text
*----------------------------------------------------------------------*
FORM get_tcode_by_tabname USING tabname
                          .

  DATA
        : lv_tcode TYPE tcode
        , lv_text TYPE ttext_stct
        , lv_param  TYPE tcdparam
        .

  CONCATENATE '%' tabname '%' INTO lv_param .




  SELECT
        tstcp~tcode
        tstct~ttext
  INTO (lv_tcode, lv_text)
  FROM
  tstcp INNER JOIN tstct ON
             tstcp~tcode = tstct~tcode
  WHERE
         tstcp~param LIKE lv_param
  .

    WRITE : / lv_tcode, lv_text.
  ENDSELECT.
ENDFORM.                    "get_tcode_by_tabname
