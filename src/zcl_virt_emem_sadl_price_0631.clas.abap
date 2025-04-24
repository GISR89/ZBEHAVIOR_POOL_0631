CLASS zcl_virt_emem_sadl_price_0631 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_sadl_exit_calc_element_read.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_virt_emem_sadl_price_0631 IMPLEMENTATION.

  METHOD if_sadl_exit_calc_element_read~get_calculation_info.

    CASE iv_entity.
      WHEN 'Z_C_CUSTOMER_TRAVEL_0631'.

        LOOP AT it_requested_calc_elements INTO DATA(ls_requested_calc_elements).

          IF ls_requested_calc_elements EQ 'PRICEWITHTAX'.
            INSERT CONV #( 'PRICE' ) INTO TABLE et_requested_orig_elements.
          ENDIF.

        ENDLOOP.

    ENDCASE.

  ENDMETHOD.

  METHOD if_sadl_exit_calc_element_read~calculate.

    DATA lt_original_data TYPE STANDARD TABLE OF z_c_customer_travel_0631 WITH DEFAULT KEY.

    lt_original_data = CORRESPONDING #( it_original_data ).

    LOOP AT lt_original_data ASSIGNING FIELD-SYMBOL(<fs_original_data>).
      <fs_original_data>-PriceWithTax = <fs_original_data>-Price * '1.15'.
    ENDLOOP.

    ct_calculated_data = CORRESPONDING #( lt_original_data ).

  ENDMETHOD.

ENDCLASS.
