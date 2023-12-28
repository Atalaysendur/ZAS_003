*&---------------------------------------------------------------------*
*& Report ZAS_003_P_ALV_BAPI
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZAS_003_P_ALV_BAPI.



INCLUDE ZAS_003_I_ALV_BAPI_TOP. "isimlendirmeyi değiştir.
INCLUDE ZAS_003_I_ALV_BAPI_CLS.
INCLUDE ZAS_003_I_ALV_BAPI_mdl.


INITIALIZATION.
  go_main = lcl_main=>create_instance( ).

START-OF-SELECTION.
  go_main->start_of_selection( ).

END-OF-SELECTION.
  go_main->end_of_selection( ).
