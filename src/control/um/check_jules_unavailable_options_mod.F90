#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt

MODULE check_jules_unavailable_options_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE check_jules_unavailable_options()

USE ereport_mod, ONLY: ereport
USE jules_print_mgr, ONLY:                                                    &
    jules_message,                                                            &
    jules_print,                                                              &
    PrNorm
USE switches_urban, ONLY: l_urban_empirical

IMPLICIT NONE

!Local variables
INTEGER :: errcode, error_sum
CHARACTER(LEN=*), PARAMETER :: RoutineName='CHECK_JULES_UNAVAILABLE_OPTIONS'


error_sum = 0
IF ( l_urban_empirical ) THEN
  error_sum = error_sum + 1
  WRITE(jules_message,'(I0,A)') error_sum,                                    &
     ": l_urban_empirical should be .false."
  CALL jules_print(RoutineName, jules_message, level = PrNorm)
END IF

IF ( error_sum > 0 ) THEN
  errcode = 30
  WRITE(jules_message,'(A,I0,A)') "One or more JULES options (", error_sum,   &
     ") have been incorrectly set for use in UM-JULES. " //                   &
     "Please see job output for details."
  CALL ereport(RoutineName, errcode, jules_message)
END IF

RETURN
END SUBROUTINE check_jules_unavailable_options
END MODULE check_jules_unavailable_options_mod
#endif
