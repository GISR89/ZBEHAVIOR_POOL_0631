CLASS lhc_Z_R_CUSTOMER_TRAVEL_0631 DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF customer_travel_status,
        open     TYPE c LENGTH 1 VALUE 'O',   " Open
        accepted TYPE c LENGTH 1 VALUE 'A',   " Accepted
        rejected TYPE c LENGTH 1 VALUE 'X',   " Rejected
      END OF customer_travel_status.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR CustomerTravel RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR CustomerTravel RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR CustomerTravel RESULT result.

    METHODS acceptClients FOR MODIFY
      IMPORTING keys FOR ACTION CustomerTravel~acceptClients RESULT result.

    METHODS rejectClients FOR MODIFY
      IMPORTING keys FOR ACTION CustomerTravel~rejectClients RESULT result.

    METHODS setDescription FOR DETERMINE ON SAVE
      IMPORTING keys FOR CustomerTravel~setDescription.

    METHODS validateCustomer FOR VALIDATE ON SAVE
      IMPORTING keys FOR CustomerTravel~validateCustomer.

ENDCLASS.

CLASS lhc_Z_R_CUSTOMER_TRAVEL_0631 IMPLEMENTATION.

  METHOD get_instance_features.

    READ ENTITIES OF z_r_customer_travel_0631 IN LOCAL MODE
    ENTITY CustomerTravel
    ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(lt_root_entity)
    FAILED failed.

    result = VALUE #( FOR ls_root_entity IN lt_root_entity (
                                     %tky = ls_root_entity-%tky
                                     %action-acceptClients = COND #(
                                  WHEN ls_root_entity-OverallStatus = customer_travel_status-open
                                  THEN if_abap_behv=>fc-o-enabled
                                  ELSE if_abap_behv=>fc-o-disabled )
                                     %action-rejectClients = COND #(
                                  WHEN ls_root_entity-OverallStatus = customer_travel_status-open
                                  THEN if_abap_behv=>fc-o-enabled
                                  ELSE if_abap_behv=>fc-o-disabled ) ) ).

  ENDMETHOD.

  METHOD get_instance_authorizations.

    DATA: lv_update_requested TYPE abap_bool,
          lv_update_granted   TYPE abap_bool,
          lv_delete_requested TYPE abap_bool,
          lv_delete_granted   TYPE abap_bool.

    READ ENTITIES OF z_r_customer_travel_0631 IN LOCAL MODE
         ENTITY CustomerTravel
         ALL FIELDS WITH CORRESPONDING #( keys )
         RESULT DATA(lt_CustomerTravel)
         FAILED failed.

    lv_update_requested = COND #(
                              WHEN requested_authorizations-%update = if_abap_behv=>mk-on OR
                                   requested_authorizations-%update = if_abap_behv=>mk-on
                              THEN abap_true
                              ELSE abap_false ).
    lv_delete_requested = COND #(
                               WHEN requested_authorizations-%delete = if_abap_behv=>mk-on
                               THEN abap_true
                               ELSE abap_false ).

    DATA(lv_technical_name) = cl_abap_context_info=>get_user_technical_name( ).

    LOOP AT lt_CustomerTravel INTO DATA(ls_CustomerTravel).
      IF lv_update_requested EQ abap_true.

        IF lv_technical_name EQ 'CB998EEE141' AND ls_CustomerTravel-CurrencyCode EQ 'USD'.
          lv_update_granted = abap_true.
        ELSE.
          lv_update_granted = abap_false.
        ENDIF.
      ENDIF.
      IF lv_delete_requested EQ abap_true.

        IF lv_technical_name EQ 'CB998EEE141' AND ls_CustomerTravel-CurrencyCode EQ 'USD'.
          lv_delete_granted = abap_true.
        ELSE.
          lv_delete_granted = abap_false.
        ENDIF.
      ENDIF.

      APPEND VALUE #( LET upd_auth = COND #( WHEN lv_update_granted EQ abap_true
           THEN if_abap_behv=>auth-allowed
           ELSE if_abap_behv=>auth-unauthorized )
      del_auth = COND #( WHEN lv_delete_granted EQ abap_true
                         THEN if_abap_behv=>auth-allowed
                         ELSE if_abap_behv=>auth-unauthorized )
                         IN
                         %tky = ls_CustomerTravel-%tky
                         %update = upd_auth
                         %action-Edit = upd_auth
                         %delete = del_auth ) TO result.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_global_authorizations.

    DATA(lv_technical_name) = cl_abap_context_info=>get_user_technical_name( ).
*create
    IF requested_authorizations-%create EQ if_abap_behv=>mk-on.

      IF lv_technical_name EQ 'CB9980000872'.

      ELSE.
        result-%create = if_abap_behv=>auth-unauthorized.
      ENDIF.

    ENDIF.
*update
    IF requested_authorizations-%update EQ if_abap_behv=>mk-on
        OR requested_authorizations-%action-Edit EQ if_abap_behv=>mk-on.

      IF lv_technical_name EQ 'CB9980000872' .
        result-%update = if_abap_behv=>auth-allowed.
        result-%action-Edit = if_abap_behv=>auth-allowed.
      ELSE.
        result-%update = if_abap_behv=>auth-unauthorized.
        result-%action-Edit = if_abap_behv=>auth-unauthorized.
      ENDIF.

    ENDIF.
*delete
    IF requested_authorizations-%delete EQ if_abap_behv=>mk-on.

      IF lv_technical_name EQ 'CB9980000872' .
        result-%delete = if_abap_behv=>auth-allowed.
      ELSE.
        result-%delete = if_abap_behv=>auth-unauthorized.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD acceptClients.

    DATA: lt_updated_root_entity TYPE TABLE FOR UPDATE z_r_customer_travel_0631,
          lv_discount            TYPE /DMO/BT_DiscountPercentage,
          lv_disc_percent        TYPE f.

    READ ENTITIES OF z_r_customer_travel_0631 IN LOCAL MODE
    ENTITY CustomerTravel
    ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(lt_root_entity)
    FAILED failed.

    LOOP AT lt_root_entity ASSIGNING FIELD-SYMBOL(<ls_root_entity>).
      lv_discount = keys[ KEY id %tky = <ls_root_entity>-%tky ]-%param-travel_discount.
      lv_disc_percent = lv_discount / 100.
      <ls_root_entity>-Price = <ls_root_entity>-Price * ( 1 - lv_disc_percent ).
      <ls_root_entity>-OverallStatus = customer_travel_status-accepted.

      APPEND VALUE #( %tky = <ls_root_entity>-%tky
                      Price = <ls_root_entity>-Price
                      OverallStatus = <ls_root_entity>-OverallStatus ) TO lt_updated_root_entity.
    ENDLOOP.

    UNASSIGN <ls_root_entity>.

    MODIFY ENTITIES OF z_r_customer_travel_0631 IN LOCAL MODE
    ENTITY CustomerTravel
    UPDATE
    FIELDS ( Price
             OverallStatus )
    WITH lt_updated_root_entity.
    FREE lt_root_entity. " Free entries in lt_root_entity

    READ ENTITIES OF z_r_customer_travel_0631 IN LOCAL MODE
    ENTITY CustomerTravel
    ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT lt_root_entity
    FAILED failed.

    RESUlt = VALUE #( FOR ls_Customer_Travel IN lt_root_entity ( %tky = ls_Customer_Travel-%tky
                                                               %param = ls_Customer_Travel ) ).

  ENDMETHOD.

  METHOD rejectClients.

    MODIFY ENTITIES OF z_r_customer_travel_0631 IN LOCAL MODE
      ENTITY CustomerTravel
      UPDATE
      FIELDS ( OverallStatus )
      WITH VALUE #(  FOR ls_key IN keys ( %tky = ls_key-%tky
                                          OverallStatus = customer_travel_status-rejected )  ).

    READ ENTITIES OF z_r_customer_travel_0631 IN LOCAL MODE
    ENTITY CustomerTravel
    ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(lt_Customer_Travel)
    FAILED failed.

    RESUlt = VALUE #( FOR ls_Customer_Travel IN lt_Customer_Travel ( %tky =
 ls_Customer_Travel-%tky
                                                                     %param = ls_Customer_Travel ) ).

  ENDMETHOD.

  METHOD setDescription.
  ENDMETHOD.

  METHOD validateCustomer.
  ENDMETHOD.

ENDCLASS.
