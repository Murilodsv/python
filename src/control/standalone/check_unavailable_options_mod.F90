#if !defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt

MODULE check_unavailable_options_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE check_unavailable_options()

USE ereport_mod, ONLY: ereport
USE jules_print_mgr, ONLY:                                                    &
    jules_message,                                                            &
    jules_print,                                                              &
    PrNorm

USE jules_surface_mod, ONLY: formdrag, no_drag, i_modiscopt, iscrntdiag,      &
                             isrfexcnvgust, l_flake_model, l_vary_z0m_soil

IMPLICIT NONE

!Local variables
INTEGER :: errcode, error_sum
CHARACTER(LEN=*), PARAMETER :: RoutineName='CHECK_UNAVAILABLE_OPTIONS'


error_sum = 0
IF ( formdrag /= no_drag ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,I0)') error_sum,                                 &
     ": formdrag should be 0 (i.e. no drag) in standalone. formdrag = ",      &
     formdrag
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF
IF ( i_modiscopt /= 0 ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,I0)') error_sum,                                 &
     ": i_modiscopt should be 0 if forcing with data at a specific " //       &
     "level, rather than a vertical average. The former is" //                &
     NEW_LINE('A') //                                                         &
     "most likely in standalone JULES. Check that this setting was " //       &
     "intended. If incorrectly set it may cause failures. i_modiscopt = ",    &
     i_modiscopt
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF
IF ( iscrntdiag /= 0 .AND. iscrntdiag /= 1 ) THEN
  ! iscrntdiag = 1 has been allowed as there are Rose stem tests that already
  ! include this, but it not recommended.
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,I0)') error_sum,                                 &
     ": It is recommended that iscrntdiag = 0 in standalone until " //        &
     "driving JULES with a decoupled variable is fully tested." //            &
     NEW_LINE('A') // "iscrntdiag = ", iscrntdiag
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF
IF ( isrfexcnvgust /= 0 ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,I0)') error_sum,                                 &
     ": isrfexcnvgust should be 0 in standlone i.e. the effects of " //       &
     "convective downdraughts on surface exchange cannot be" //               &
     NEW_LINE('A') // "included. isrfexcnvgust = ", isrfexcnvgust
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF
IF ( l_flake_model ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                 &
     ": Flake is currently not available to standalone. l_flake_model = ",    &
     l_flake_model
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF
IF ( l_vary_z0m_soil ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A,L1)') error_sum,                                 &
     ": Variable roughness length of bare soil currently not available" //    &
     " to standalone. l_vary_z0m_soil = ", l_vary_z0m_soil
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

IF ( error_sum > 0 ) THEN
  errcode = 10
  WRITE(jules_message,'(A,I0,A)') "One or more JULES options (", error_sum,   &
     ") have been incorrectly set for use in JULES standalone." //            &
     NEW_LINE('A') // "Please see job output for details."
  CALL ereport(RoutineName, errcode, jules_message)
END IF

IF ( iscrntdiag /= 0 ) THEN
  ! iscrntdiag = 1 has been allowed as there are Rose stem tests that already
  ! include this, but it not recommended.
  errcode = -10
  WRITE(jules_message,'(A,I0)')                                               &
     "It is recommended that iscrntdiag = 0 in standalone until " //          &
     "driving JULES with a decoupled variable is fully tested." //            &
     NEW_LINE('A') // "iscrntdiag = ", iscrntdiag
  CALL ereport(RoutineName, errcode, jules_message)
END IF

RETURN
END SUBROUTINE check_unavailable_options
END MODULE check_unavailable_options_mod
#endif
