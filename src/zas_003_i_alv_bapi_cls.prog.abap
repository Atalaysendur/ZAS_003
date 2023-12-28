*&---------------------------------------------------------------------*
*& Include          ZAS_001_I_ALV_BAPI_CLS
*&---------------------------------------------------------------------*


CLASS lcl_main DEFINITION CREATE PRIVATE FINAL.

  PUBLIC SECTION.
    METHODS: get_data.
    METHODS start_of_selection.
    METHODS end_of_selection.
    METHODS sas_create.
    METHODS display_alv.
    METHODS display_alv_kalem.
    METHODS handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.
    METHODS handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object e_interactive.
    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id e_column_id.
    METHODS handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row e_column es_row_no.
    METHODS handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed.

    CLASS-METHODS: create_instance RETURNING VALUE(ro_main) TYPE REF TO lcl_main.
*
  PRIVATE SECTION.
    DATA lt_fatura_log    TYPE TABLE OF zas_003_t_log.
    DATA lt_bapiitem      TYPE TABLE OF bapimepoitem.
    DATA lt_bapiitemx     TYPE TABLE OF bapimepoitemx.
    DATA lt_bapiaccount   TYPE TABLE OF bapimepoaccount.
    DATA lt_bapiaccountx  TYPE TABLE OF bapimepoaccountx.
    DATA lt_fatura_baslik TYPE TABLE OF zas_003_s_fatura_baslik.
    DATA lt_fatura_kalem  TYPE TABLE OF zas_003_s_fatura_kalem.
    DATA lS_fatura_log    TYPE zas_003_t_log.
    DATA ls_fatura_baslik TYPE zas_003_s_fatura_baslik.
    DATA ls_fatura_kalem  TYPE zas_003_s_fatura_kalem.
    DATA ls_cell_color    TYPE lvc_s_scol.
    DATA lv_fcat          TYPE dd02l-tabname.
    DATA lt_rows          TYPE lvc_t_row.
    DATA wa_rows          TYPE lvc_s_row.
    CLASS-DATA: mo_main TYPE REF TO lcl_main,
                mo_spli TYPE REF TO cl_gui_splitter_container,
                mo_alv  TYPE REF TO cl_gui_alv_grid,
                mo_alv2 TYPE REF TO cl_gui_alv_grid,
                mo_cont TYPE REF TO cl_gui_custom_container,
                mo_sub1 TYPE REF TO cl_gui_container,
                mo_sub2 TYPE REF TO cl_gui_container.

    METHODS set_excluding        RETURNING VALUE(rt_excluding)   TYPE ui_functions.
    METHODS main_fieldcat_baslik RETURNING VALUE(rt_fcat_baslik) TYPE lvc_t_fcat.
    METHODS main_fieldcat_kalem  RETURNING VALUE(rt_fcat_kalem)  TYPE lvc_t_fcat.
    METHODS fill_main_layout     RETURNING VALUE(rs_layo)        TYPE lvc_s_layo.

ENDCLASS.


CLASS lcl_main IMPLEMENTATION.

  METHOD create_instance.
    IF mo_main IS INITIAL.
      mo_main = NEW #( ).
    ENDIF.
    ro_main = mo_main.
  ENDMETHOD.

  METHOD get_data.

    SELECT vbrk~vbeln,
           vbrk~bukrs,
           vbrk~fkdat,
           vbrk~fkart,
           vbrk~kunrg,
           vbrk~fktyp,
           vbrk~vkorg ,
           vbrk~vtweg,
           vbrk~kunrg AS kunrg2,
           vbrk~netwr,
           vbrk~waerk
      FROM vbrk
      INNER JOIN vbrp ON vbrk~vbeln EQ vbrp~vbeln
      INTO TABLE @lt_fatura_baslik
      WHERE vbrk~vbeln IN @s_vbeln AND
            vbrk~fkdat IN @s_fkdat AND
            vbrk~fkart IN @s_fkart AND
            vbrk~kunrg IN @s_fkdat.

  ENDMETHOD.

  METHOD start_of_selection.

    mo_main->get_data( ).

  ENDMETHOD.

  METHOD end_of_selection.

*    IF ( s_vbeln <> 0  OR s_fkart <> 0 OR s_fkdat <> 0 OR s_kunrg <> 0 ) AND lt_fatura_baslik IS INITIAL.
*      MESSAGE i000(zas_003).
*    ELSE.
    CALL SCREEN 0100.
*    ENDIF.

  ENDMETHOD.

  METHOD sas_create.

    DATA: lv_po_item_count TYPE ebelp.
    DATA: lr_vbeln TYPE RANGE OF lips-vbeln.
    DATA: lr_posnr TYPE RANGE OF lips-posnr.
    DATA: lt_tab TYPE esp1_message_tab_type,
          ls_tab TYPE esp1_message_wa_type.

    CALL METHOD mo_alv2->get_selected_rows
      IMPORTING
        et_index_rows = lt_rows.


    IF lt_rows[] IS NOT INITIAL.

      READ TABLE lt_rows INTO wa_rows   INDEX 1.
      READ TABLE lt_fatura_kalem INTO ls_fatura_kalem   INDEX wa_rows-index.

      SELECT SINGLE kunag, waerk
        FROM vbrk
        WHERE vbeln EQ  @ls_fatura_kalem-vbeln
        INTO @DATA(ls_kunag_waerk).

      DATA: lt_return TYPE TABLE OF bapiret2.
      DATA(ls_bapiheader) = VALUE bapimepoheader( comp_code = ls_kunag_waerk-kunag
                                                  doc_type = 'NB'
                                                  purch_org = gs_sas-ekorg
                                                  pur_group = gs_sas-ekgrp
                                                  currency  = ls_kunag_waerk-waerk ).

      DATA(ls_bapiheaderx) = VALUE bapimepoheaderx(  comp_code = abap_true
                                                    doc_type = abap_true
                                                    purch_org = abap_true
                                                    pur_group = abap_true
                                                    currency  = abap_true ).



      LOOP AT lt_rows INTO wa_rows.

        lv_po_item_count += 10.
        READ TABLE lt_fatura_kalem ASSIGNING FIELD-SYMBOL(<fss_fatura_kalem>)   INDEX wa_rows-index.
        READ TABLE lt_fatura_baslik INTO ls_fatura_baslik WITH KEY vbeln = ls_fatura_kalem-vbeln.


        lt_bapiitem = VALUE #( BASE lt_bapiitem ( po_item    = lv_po_item_count
                                                  material   = ls_fatura_kalem-matnr
                                                  short_text = ls_fatura_kalem-arktx
                                                  plant      = gs_sas-werks
                                                  stge_loc   = gs_sas-werks
                                                  matl_group = 'X'
                                                  quantity   = ls_fatura_kalem-fkimg
                                                  po_unit    = ls_fatura_kalem-vrkme
                                                  info_upd   = 'X'
                                                  pckg_no    = '01' ) ).


        lt_bapiitemx = VALUE #( BASE lt_bapiitemx ( po_item    = abap_true
                                                    material   = abap_true
                                                    short_text = abap_true
                                                    plant      = abap_true
                                                    stge_loc   = abap_true
                                                    matl_group = abap_true
                                                    quantity   = abap_true
                                                    po_unit    = abap_true
                                                    info_upd   = abap_true
                                                    pckg_no    = abap_true  ) ).


        lt_bapiaccount = VALUE #( BASE lt_bapiaccount ( po_item   = lv_po_item_count
                                                        serial_no = '01'  ) ).

        lt_bapiaccountx = VALUE #( BASE lt_bapiaccountx ( po_item  = abap_true
                                                          serial_no = abap_true
                                                          ) ).


        "BAPI çalışmadığı için örnek ebeln.
        <fss_fatura_kalem>-ebeln = '4500000390'.



      ENDLOOP.
      mo_alv2->refresh_table_display( is_stable = VALUE #( col = abap_true row = abap_true ) ).
    ENDIF.

    IF gs_sas-ekgrp IS INITIAL OR gs_sas-ekorg IS INITIAL OR gs_sas-lgort IS INITIAL AND gs_sas-werks IS INITIAL.
      MESSAGE e001(zas_003).
    ELSE.
      CALL FUNCTION 'BAPI_PO_CREATE1'
        EXPORTING
          poheader   = ls_bapiheader
          poheaderx  = ls_bapiheaderx
        TABLES
          return     = lt_return
          poitem     = lt_bapiitem
          poitemx    = lt_bapiitemx
          poaccount  = lt_bapiaccount
          poaccountx = lt_bapiaccountx.


      IF lt_return IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        MESSAGE i002(zas_003).
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        LOOP AT lt_return INTO DATA(ls_return).

          IF ls_return-type EQ 'E'.
            lt_tab = VALUE #( BASE lt_tab ( msgid  = ls_return-number
                                            msgno  = ls_return-log_msg_no
                                            msgty  = 'E'
                                            msgv1  = ls_return-message
                                            lineno = sy-tabix ) ).
          ELSEIF ls_return-type EQ 'W'.
            lt_tab = VALUE #( BASE lt_tab ( msgid  = ls_return-number
                                            msgno  = ls_return-log_msg_no
                                            msgty  = 'E'
                                            msgv1  = ls_return-message
                                            lineno = sy-tabix ) ).
          ENDIF.

        ENDLOOP.
      ENDIF.



      CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
        TABLES
          i_message_tab = lt_tab.

    ENDIF.


  ENDMETHOD.

  METHOD main_fieldcat_baslik.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZAS_003_S_FATURA_BASLIK'
      CHANGING
        ct_fieldcat      = rt_fcat_baslik.

    LOOP AT rt_fcat_baslik ASSIGNING FIELD-SYMBOL(<fs_fcat_baslik>).
      <fs_fcat_baslik>-colddictxt = 'L'.
      CASE <fs_fcat_baslik>-fieldname.
        WHEN 'VBELN' .
          <fs_fcat_baslik>-hotspot = abap_true.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD main_fieldcat_kalem.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZAS_003_S_FATURA_KALEM'
      CHANGING
        ct_fieldcat      = rt_fcat_kalem.


    LOOP AT rt_fcat_kalem ASSIGNING FIELD-SYMBOL(<fs_fcat_kalem>).
      <fs_fcat_kalem>-colddictxt = 'L'.
      CASE <fs_fcat_kalem>-fieldname.
        WHEN 'VBELN' .
          <fs_fcat_kalem>-hotspot = abap_true.
        WHEN 'NETWR3' .
          <fs_fcat_kalem>-edit = abap_true.
        WHEN 'LOG_DISPLAY'.
          <fs_fcat_kalem>-hotspot = abap_true.
        WHEN 'EBELN'.
          <fs_fcat_kalem>-hotspot = abap_true.
      ENDCASE.
    ENDLOOP.


    LOOP AT lt_fatura_kalem ASSIGNING FIELD-SYMBOL(<fs_kalem>) WHERE netwr3 LE 100.
      <fs_kalem>-cellstyle = VALUE #( ( fieldname = 'NETWR3'
                                        style = cl_gui_alv_grid=>mc_style_disabled ) ) .
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_main_layout.

    rs_layo = VALUE lvc_s_layo( zebra       = abap_true
                                sel_mode    = 'A'
                                cwidth_opt  = abap_true
                                stylefname  = 'CELLSTYLE'
                                ctab_fname  = 'CELL_COLOR'
                                                            ).

  ENDMETHOD.

  METHOD set_excluding.
    rt_excluding = VALUE #( ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
                            ( cl_gui_alv_grid=>mc_fc_loc_delete_row )  ).
  ENDMETHOD.

  METHOD handle_double_click.

    REFRESH: lt_fatura_kalem.

    READ TABLE lt_fatura_baslik INTO ls_fatura_baslik INDEX e_row-index.

    IF sy-subrc IS INITIAL.

      SELECT vbrp~vbeln,
             vbrp~posnr,
             vbrp~fkimg,
             vbrp~vrkme,
             vbrp~meins,
             vbrp~waerk,
             vbrp~ntgew ,
             vbrp~brgew,
             vbrp~netwr,
             vbrp~matnr,
             vbrp~arktx,
             vbrp~fkimg AS fkimg2
        FROM vbrp
        INNER JOIN vbrk ON vbrk~vbeln EQ vbrp~vbeln
        INTO CORRESPONDING FIELDS OF TABLE @lt_fatura_kalem
        WHERE vbrk~vbeln EQ @ls_fatura_baslik-vbeln.

      LOOP AT lt_fatura_kalem ASSIGNING FIELD-SYMBOL(<fs_kalem2>).
        <fs_kalem2> = VALUE #( BASE <fs_kalem2>
                                    log_display = '@96@'
                                    netwr3 = <fs_kalem2>-netwr / <fs_kalem2>-fkimg ).
      ENDLOOP.

      CLEAR: ls_fatura_log.

      SELECT  VBELN,
              POSNR,
              DATUM,
              UZEIT,
              NET_FIYAT,
              NET_FIYAT_GNCL,
              UNAME
      FROM zas_003_t_log
      INTO TABLE @DATA(lt_fatura_log2)
      WHERE vbeln EQ @ls_fatura_baslik-vbeln.

      SORT lt_fatura_log2 BY  datum DESCENDING  uzeit DESCENDING .
      DELETE ADJACENT DUPLICATES FROM lt_fatura_log2 COMPARING vbeln posnr.

      LOOP AT lt_fatura_log2 INTO DATA(ls_fatura_log2).

        READ TABLE lt_fatura_kalem ASSIGNING <fs_kalem2> WITH KEY vbeln = ls_fatura_log2-vbeln
                                                                  posnr  = ls_fatura_log2-posnr .
        IF sy-subrc EQ 0.

          <fs_kalem2> = VALUE #( BASE <fs_kalem2>
                                      netwr3 = ls_fatura_log2-net_fiyat_gncl
                                      netwr2 = ls_fatura_log2-net_fiyat_gncl
                                      netwr = <fs_kalem2>-netwr3 * <fs_kalem2>-fkimg
                                      cell_color = VALUE #( fname = 'NETWR2' ( color-col = '6'
                                                                               color-int = '1'
                                                                               color-inv = '0' ) ) ).
        ENDIF.

      ENDLOOP.


    ENDIF.

    mo_main->display_alv_kalem( ).

  ENDMETHOD.

  METHOD handle_toolbar.


    APPEND VALUE stb_button( butn_type = 0
                       function  = '&KAYDET'
                       icon      = icon_system_save
                       text      = 'Kaydet'
                       quickinfo = 'Kaydet'
                       disabled  = ''
                      ) TO e_object->mt_toolbar.

    APPEND VALUE stb_button( butn_type = 0
                   function  = '&SAS'
                   icon      = icon_create
                   text      = 'SAS Oluştur'
                   quickinfo = 'SAS Oluştur'
                   disabled  = ''
                  ) TO e_object->mt_toolbar.


  ENDMETHOD.

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN '&KAYDET'.

        DATA: lt_fatura_log_KYDT TYPE TABLE OF zas_003_t_log.

        CLEAR:  lt_rows, wa_rows.

        CALL METHOD mo_alv2->get_selected_rows
          IMPORTING
            et_index_rows = lt_rows.

        IF lt_rows[] IS NOT INITIAL.

          LOOP AT lt_rows INTO wa_rows.
            READ TABLE lt_fatura_kalem INTO ls_fatura_kalem INDEX wa_rows-index.

            IF sy-subrc IS INITIAL.

              APPEND VALUE #( vbeln          = ls_fatura_kalem-vbeln
                              posnr          = ls_fatura_kalem-posnr
                              datum          = sy-datum
                              uzeit          = sy-uzeit
                              net_fiyat      = ls_fatura_kalem-netwr2
                              net_fiyat_gncl = ls_fatura_kalem-netwr3
                              uname          = sy-uname               ) TO lt_fatura_log_KYDT.

            ENDIF.
          ENDLOOP.

          INSERT zas_003_t_log  FROM TABLE lt_fatura_log_KYDT.
          IF sy-subrc IS INITIAL.
            COMMIT WORK.
            MESSAGE i002(zas_003).
            REFRESH : lt_fatura_log_KYDT,lt_rows.
          ELSE.
            ROLLBACK WORK.
            MESSAGE i003(zas_003).
            REFRESH : lt_fatura_log_KYDT,lt_rows.
          ENDIF.

        ELSE.
          MESSAGE i004(zas_003).
        ENDIF.

      WHEN '&SAS'.

        CALL SCREEN 0200 STARTING AT 60 6
                         ENDING AT 120 20.


    ENDCASE.
    mo_alv2->refresh_table_display( is_stable = VALUE #( col = abap_true row = abap_true ) ).
  ENDMETHOD.

  METHOD handle_data_changed.

    DATA: ls_good TYPE lvc_s_modi.
    DATA: lv_data TYPE char50.
    FIELD-SYMBOLS: <fs> TYPE any.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good.

      READ TABLE lt_fatura_kalem INTO ls_fatura_kalem INDEX ls_good-row_id.
      CONCATENATE 'LS_FATURA_KALEM-' ls_good-fieldname INTO lv_data.
      ASSIGN (lv_data) TO <fs>.
      <fs> = ls_good-value.

      APPEND VALUE #( vbeln          = ls_fatura_kalem-vbeln
                      posnr          = ls_fatura_kalem-posnr
                      datum          = sy-datum
                      uzeit          = sy-uzeit
                      net_fiyat      = ls_fatura_kalem-netwr / ls_fatura_kalem-fkimg2
                      net_fiyat_gncl = ls_good-value
                      uname          = sy-uname ) TO lt_fatura_log.

      ls_fatura_kalem = VALUE #( BASE ls_fatura_kalem
                                       netwr  = ls_fatura_kalem-fkimg2 * ls_good-value
                                       netwr2 = ls_good-value
                                       cell_color = VALUE #( fname = 'NETWR2' ( color-col = '6'
                                                                                color-int = '1'
                                                                                color-inv = '0' ) ) ).

      MODIFY lt_fatura_kalem FROM ls_fatura_kalem INDEX ls_good-row_id.
    ENDLOOP.
    mo_alv2->refresh_table_display( is_stable = VALUE #( col = abap_true row = abap_true ) ).
  ENDMETHOD.

  METHOD handle_hotspot_click.
    CASE e_column_id.
      WHEN 'VBELN'.
        READ TABLE lt_fatura_baslik INTO ls_fatura_baslik INDEX e_row_id.
        SET PARAMETER ID 'VF' FIELD ls_fatura_baslik-vbeln.
        CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
      WHEN 'LOG_DISPLAY'.

        DATA lt_message TYPE esp1_message_tab_type.

        READ TABLE lt_fatura_kalem INTO ls_fatura_kalem INDEX e_row_id.

        SELECT  vbeln,
                posnr,
                datum,
                uname,
                net_fiyat,
                net_fiyat_gncl
         FROM zas_003_t_log
          INTO TABLE @DATA(lt_log_msg)
          WHERE vbeln EQ @ls_fatura_kalem-vbeln AND
                posnr EQ @ls_fatura_kalem-posnr.

        LOOP AT lt_log_msg INTO DATA(ls_log_msg).
          DATA(lv_msg) = |Net Değer : { CONV string( ls_log_msg-net_fiyat ) } Güncel Değer : { CONV string( ls_log_msg-net_fiyat_gncl ) }|.

          APPEND VALUE #(  msgid = 'E4'
                           msgty = 'S'
                           msgv1 = lv_msg
                           lineno = sy-tabix ) TO lt_message.

        ENDLOOP.

        CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
          TABLES
            i_message_tab = lt_message.

      WHEN 'EBELN'.

        READ TABLE lt_fatura_kalem INTO ls_fatura_kalem INDEX e_row_id.
        IF ls_fatura_kalem-ebeln IS NOT INITIAL.
          SET PARAMETER ID 'BES' FIELD ls_fatura_kalem-ebeln.
          CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
        ENDIF.


    ENDCASE.

  ENDMETHOD.

  METHOD display_alv.

    IF mo_alv IS INITIAL.

      DATA(lt_fcat_baslik) = me->main_fieldcat_baslik( ).


      mo_cont  = NEW #( container_name = 'CC_0100' ).

      mo_spli  = NEW #( parent  = mo_cont
                        rows    = 2
                        columns = 1 ).


      mo_spli->get_container( EXPORTING
                              row       = 1
                              column    = 1
                              RECEIVING container = mo_sub1 ).
*
      mo_spli->set_row_height( EXPORTING
                               id       = 2
                               height   = 0 ).


      mo_alv   = NEW #( i_parent =  mo_sub1  ).


      SET HANDLER:
                  me->handle_double_click  FOR mo_alv,
                  me->handle_hotspot_click FOR mo_alv.

      CALL METHOD mo_alv->set_table_for_first_display
        EXPORTING
          is_layout                     = me->fill_main_layout( )
          it_toolbar_excluding          = me->set_excluding( )
        CHANGING
          it_outtab                     = lt_fatura_baslik
          it_fieldcatalog               = lt_fcat_baslik
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.



    ELSE.
      mo_alv->refresh_table_display( is_stable = VALUE #( col = abap_true row = abap_true ) ).

    ENDIF.

  ENDMETHOD.

  METHOD display_alv_kalem.


    IF mo_alv2 IS INITIAL.

      DATA(lt_fcat_kalem) = me->main_fieldcat_kalem( ).


      mo_spli->get_container( EXPORTING
                  row       = 2
                  column    = 1
                  RECEIVING container = mo_sub2 ).


      mo_alv2   = NEW #( i_parent = mo_sub2  ).


      mo_spli->set_row_height( EXPORTING
                         id       = 2
                         height   = 50 ).


      SET HANDLER: me->handle_user_command  FOR mo_alv2,
                   me->handle_toolbar       FOR mo_alv2,
                   me->handle_data_changed  FOR mo_alv2,
                   me->handle_hotspot_click FOR mo_alv2.

      CALL METHOD mo_alv2->set_table_for_first_display
        EXPORTING
          is_layout                     = me->fill_main_layout( )
          it_toolbar_excluding          = me->set_excluding( )
        CHANGING
          it_outtab                     = lt_fatura_kalem
          it_fieldcatalog               = lt_fcat_kalem
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.

      mo_alv2->set_ready_for_input( i_ready_for_input = 1 ). " Önemli

      CALL METHOD mo_alv2->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

      CALL METHOD mo_alv2->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    ELSE.
      mo_alv2->refresh_table_display( is_stable = VALUE #( col = abap_true row = abap_true ) ).

    ENDIF.

  ENDMETHOD.



ENDCLASS.
