*&---------------------------------------------------------------------*
*& Include          ZAS_001_I_ALV_BAPI_TOP
*&---------------------------------------------------------------------*

TABLES: vbrk.
TYPE-POOLS: icon.
CLASS lcl_main DEFINITION DEFERRED.
DATA: go_main TYPE REF TO lcl_main.

TYPES: BEGIN OF gty_sas,
         ekorg TYPE ekko-ekorg,
         ekgrp TYPE ekko-ekgrp,
         werks TYPE ewerk,
         lgort TYPE lgort_d,
       END OF gty_sas.


DATA: gs_sas TYPE gty_sas.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_vbeln FOR vbrk-vbeln.
  SELECT-OPTIONS: s_fkdat FOR vbrk-fkdat.
  SELECT-OPTIONS: s_fkart FOR vbrk-fkart.
  SELECT-OPTIONS: s_kunrg FOR vbrk-kunrg.
SELECTION-SCREEN END OF BLOCK blk1.
