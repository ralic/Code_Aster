subroutine niinit(nomte, typmod, ndim, nno1, nno2,&
                  nno3, nno4, vu, vg, vp,&
                  vpi)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: sebastien.fayolle at edf.fr
    implicit none
!
#include "asterfort/teattr.h"
#include "asterfort/utmess.h"
    integer :: ndim, nno1, nno2, nno3, nno4
    integer :: vu(3, 27), vg(27), vp(27), vpi(3, 27), iret, iefm
    character(len=8) :: typmod(*)
    character(len=16) :: nomte, alias
! ----------------------------------------------------------------------
!        INITIALISATION POUR LES ELEMENTS QUASI-INCOMPRESSIBLES
! ----------------------------------------------------------------------
! IN  NOMTE     NOM DE L'ELEMENT
! IN  TYPMOD    TYPE DE MODELISATION
! IN  NDIM      DIMENSION DE L'ESPACE
! IN  NNO1      NOMBRE DE NOEUDS POUR L'ELEMENT PORTANT LES DEPLACEMENTS
! IN  NNO2      NOMBRE DE NOEUDS POUR L'ELEMENT PORTANT LES GONFELEMENTS
! IN  NNO3      NOMBRE DE NOEUDS POUR L'ELEMENT PORTANT LES PRESSIONS
! IN  NNO4      NOMBRE DE NOEUDS POUR L'ELEMENT PORTANT LES GRADIENTS DE
!               PRESSIONS
! OUT VU      : TABLEAU DES INDICES DES DDL DE DEPLACEMENTS
! OUT VG      : TABLEAU DES INDICES DES DDL DE GONFELEMENTS
! OUT VP      : TABLEAU DES INDICES DES DDL DE PRESSION
! OUT VPI     : TABLEAU DES INDICES DES DDL DE GRADIENTS DE PRESSION
! LOC IEFM : INDENTIFIANT DE L ELEMENT FINI MIXTE : 1 = P2-P1-P1
!                                                   2 = P2-P1-P2
!                                                   3 = P2-P0-P0
!                                                   4 = P2-P1-CR
!                                                   5 = P2-P2-P2
!                                                   6 = P1-P1-P1
!                                                   7 = P1+-P1
!                                                   8 = P2-P1
!                                                   9 = P1 OSGS-P1
!
! RM : POUR L INSTANT ON NE TRAITE QUE LES ELEMENTS P2-P1-P1 ET P2-P1-P2
!      A TERME IL FAUDRA ACTER DE L ELEMENT LE PLUS EFFICACE
! ----------------------------------------------------------------------
    integer :: n, os
! ----------------------------------------------------------------------
!
!    RECUPERATION DU TYPE D'ELEMENT VIA L'ALIAS
    call teattr('S', 'ALIAS8', alias, iret)
!
    if (nno2 .ne. 0) then
!       ON EST DANS LE CAS DES FORMULATIONS A 3 CHAMPS
        if (nno1 .eq. nno3) then
!       P2.PX.P2 OU P1.PX.P1
!        IF(NNO1 .EQ. NNO2)THEN
!       P2.P2.P2 OU P1.P1.P1
!          IF (ALIAS(6:8).EQ.'T10')  IEFM = 5
!          IF (ALIAS(6:8).EQ.'H20')  IEFM = 5
!          IF (ALIAS(6:8).EQ.'P15')  IEFM = 5
!          IF (ALIAS(6:8).EQ.'TR6')  IEFM = 5
!          IF (ALIAS(6:8).EQ.'QU8')  IEFM = 5
!          IF (ALIAS(6:8).EQ.'TE4')  IEFM = 6
!          IF (ALIAS(6:8).EQ.'HE8')  IEFM = 6
!          IF (ALIAS(6:8).EQ.'PE6')  IEFM = 6
!          IF (ALIAS(6:8).EQ.'TR3')  IEFM = 6
!          IF (ALIAS(6:8).EQ.'QU4')  IEFM = 6
!        ELSE
!       P2.P1.P2
!            IF (ALIAS(6:8).EQ.'T10')  IEFM = 2
!            IF (ALIAS(6:8).EQ.'H20')  IEFM = 2
!            IF (ALIAS(6:8).EQ.'P15')  IEFM = 2
!            IF (ALIAS(6:8).EQ.'TR6')  IEFM = 2
!            IF (ALIAS(6:8).EQ.'QU8')  IEFM = 2
            iefm = 2
!        ENDIF
        else if (nno2 .eq. nno3) then
!       PX.P1.P1 OU PX.P0.P0
!          IF (ALIAS(6:8).EQ.'T10')  IEFM = 1
!          IF (ALIAS(6:8).EQ.'H20')  IEFM = 1
!          IF (ALIAS(6:8).EQ.'P15')  IEFM = 1
!          IF (ALIAS(6:8).EQ.'TR6')  IEFM = 1
!          IF (ALIAS(6:8).EQ.'QU8')  IEFM = 1
            iefm = 1
!       2D-P2.P0.P0
!        IF (ALIAS(6:8).EQ.'TR7')  IEFM = 3
!      ELSE
!       3D-P2.P1.CR
!        IF (ALIAS(6:8).EQ.'T14')  IEFM = 4
        endif
    else
        if (nno4 .eq. 0) then
!       ON EST DANS LE CAS DES FORMULATIONS A 2 CHAMPS
            if (nno1 .eq. nno3) then
!       P1+.P1
                iefm = 7
            else
!       P2.P1
                iefm = 8
            endif
        else
!       P1 OSGS.P1
            iefm = 9
        endif
    endif
!
!
    if (ndim .eq. 3) then
        if (iefm .eq. 1) then
!       3D-P2.P1.P1
            do n = 1, nno2
                vu(1,n) = 1 + (n-1)*5
                vu(2,n) = 2 + (n-1)*5
                vu(3,n) = 3 + (n-1)*5
                vp( n) = 4 + (n-1)*5
                vg( n) = 5 + (n-1)*5
            end do
            os = 5*nno2
            do n = 1, nno1-nno2
                vu(1,n+nno2) = 1 + (n-1)*3 + os
                vu(2,n+nno2) = 2 + (n-1)*3 + os
                vu(3,n+nno2) = 3 + (n-1)*3 + os
            end do
            goto 100
        else if (iefm .eq. 2) then
!       3D-P2.P1.P2
            do n = 1, nno2
                vu(1,n) = 1 + (n-1)*5
                vu(2,n) = 2 + (n-1)*5
                vu(3,n) = 3 + (n-1)*5
                vp( n) = 4 + (n-1)*5
                vg( n) = 5 + (n-1)*5
            end do
            os = 5*nno2
            do n = 1, nno1-nno2
                vu(1,n+nno2) = 1 + (n-1)*4 + os
                vu(2,n+nno2) = 2 + (n-1)*4 + os
                vu(3,n+nno2) = 3 + (n-1)*4 + os
                vp( n+nno2) = 4 + (n-1)*4 + os
            end do
            goto 100
!       ELSEIF (IEFM .EQ. 4) THEN
!       3D-P2.P1.CR
!         DO 50 N = 1,NNO2
!           DO 60 I = 1,NDIM
!             VU(I,N) = I + (N-1)*(NDIM+1)
!  60       CONTINUE
!           VG(N) = 1 + NDIM + (N-1)*(NDIM+1)
!  50     CONTINUE
!         OS = (1+NDIM)*NNO2
!         DO 70 N = 1,NNO1-NNO2
!           DO 80 I = 1,NDIM
!             VU(I,N+NNO2) = I + (N-1)*(NDIM) + OS
!  80       CONTINUE
!  70     CONTINUE
!         OS = NNO1*NDIM + NNO2
!         DO 90 N = 1,NNO3
!           VP(N) = OS + N
!  90     CONTINUE
!         GOTO 100
!        ELSEIF (IEFM .EQ. 5 .OR. IEFM .EQ. 6) THEN
!       3D-P2.P2.P2 ET 3D-P1.P1.P1
!          DO 100 N = 1,NNO1
!            VU(1,N) = 1 + (N-1)*5
!            VU(2,N) = 2 + (N-1)*5
!            VU(3,N) = 3 + (N-1)*5
!            VP(N)   = 4 + (N-1)*5
!            VG(N)   = 5 + (N-1)*5
! 100      CONTINUE
!          GOTO 100
        else if (iefm .eq. 7) then
!       3D-P1+.P1
            do n = 1, nno1
                vu(1,n) = 1 + (n-1)*4
                vu(2,n) = 2 + (n-1)*4
                vu(3,n) = 3 + (n-1)*4
                vp( n) = 4 + (n-1)*4
            end do
            goto 100
        else if (iefm .eq. 8) then
!       3D-P2.P1
            do n = 1, nno3
                vu(1,n) = 1 + (n-1)*4
                vu(2,n) = 2 + (n-1)*4
                vu(3,n) = 3 + (n-1)*4
                vp( n) = 4 + (n-1)*4
            end do
            os = 4*nno3
            do n = 1, nno1-nno3
                vu(1,n+nno3) = 1 + (n-1)*3 + os
                vu(2,n+nno3) = 2 + (n-1)*3 + os
                vu(3,n+nno3) = 3 + (n-1)*3 + os
            end do
            goto 100
        else if (iefm .eq. 9) then
!       3D-P1 OSGS.P1
            do n = 1, nno1
                vu( 1,n) = 1 + (n-1)*7
                vu( 2,n) = 2 + (n-1)*7
                vu( 3,n) = 3 + (n-1)*7
                vp( n) = 4 + (n-1)*7
                vpi(1,n) = 5 + (n-1)*7
                vpi(2,n) = 6 + (n-1)*7
                vpi(3,n) = 7 + (n-1)*7
            end do
            goto 100
        endif
    else if (ndim .eq. 2) then
        if (iefm .eq. 1) then
!       2D-P2.P1.P1
            do n = 1, nno2
                vu(1,n) = 1 + (n-1)*4
                vu(2,n) = 2 + (n-1)*4
                vu(3,n) = 0
                vp(n) = 3 + (n-1)*4
                vg(n) = 4 + (n-1)*4
            end do
            os = 4*nno2
            do n = 1, nno1-nno2
                vu(1,n+nno2) = 1 + (n-1)*2 + os
                vu(2,n+nno2) = 2 + (n-1)*2 + os
            end do
            goto 100
        else if (iefm .eq. 2) then
!       2D-P2.P1.P2
            do n = 1, nno2
                vu(1,n) = 1 + (n-1)*4
                vu(2,n) = 2 + (n-1)*4
                vu(3,n) = 0
                vp(n) = 3 + (n-1)*4
                vg(n) = 4 + (n-1)*4
            end do
            os = 4*nno2
            do n = 1, nno1-nno2
                vu(1,n+nno2) = 1 + (n-1)*3 + os
                vu(2,n+nno2) = 2 + (n-1)*3 + os
                vu(3,n) = 0
                vp(n+nno2) = 3 + (n-1)*3 + os
            end do
            goto 100
!       ELSEIF (IEFM .EQ. 3) THEN
!       2D-P2.P0.P0
!         DO 150 N = 1,NNO1
!           VU(1,N) = 1 + (N-1)*2
!           VU(2,N) = 2 + (N-1)*2
!  150    CONTINUE
!         OS = 2*NNO1
!         VP(1) = OS+1
!         VG(1) = OS+2
!         GOTO 100
!       ELSEIF (IEFM .EQ. 4) THEN
!       2D-P2.P1.CR
!         DO 160 N = 1,NNO2
!           VU(1,N) = 1 + (N-1)*3
!           VU(2,N) = 2 + (N-1)*3
!           VG(N)   = 3 + (N-1)*3
!  160    CONTINUE
!         OS = 3*NNO2
!         DO 170 N = 1,NNO1-NNO2
!           VU(1,N+NNO2) = 1 + (N-1)*3 + OS
!           VU(2,N+NNO2) = 2 + (N-1)*3 + OS
!           VP(N)        = 3 + (N-1)*3 + OS
!  170    CONTINUE
!         GOTO 100
!        ELSEIF (IEFM .EQ. 5 .OR. IEFM .EQ. 6) THEN
!       2D-P2.P2.P2 ET 2D-P1.P1.P1
!          DO 180 N = 1,NNO1
!            VU(1,N) = 1 + (N-1)*4
!            VU(2,N) = 2 + (N-1)*4
!            VU(3,N) = 0
!            VP(N)   = 3 + (N-1)*4
!            VG(N)   = 4 + (N-1)*4
! 180      CONTINUE
!          GOTO 100
        else if (iefm .eq. 7) then
!       2D-P1+.P1
            do n = 1, nno1
                vu(1,n) = 1 + (n-1)*3
                vu(2,n) = 2 + (n-1)*3
                vu(3,n) = 0
                vp(n) = 3 + (n-1)*3
            end do
            goto 100
        else if (iefm .eq. 8) then
!       2D-P2.P1
            do n = 1, nno3
                vu(1,n) = 1 + (n-1)*3
                vu(2,n) = 2 + (n-1)*3
                vu(3,n) = 0
                vp(n) = 3 + (n-1)*3
            end do
            os = 3*nno3
            do n = 1, nno1-nno3
                vu(1,n+nno3) = 1 + (n-1)*2 + os
                vu(2,n+nno3) = 2 + (n-1)*2 + os
                vu(3,n+nno3) = 0
            end do
            goto 100
        else if (iefm .eq. 9) then
!       2D-P1 OSGS.P1
            do n = 1, nno1
                vu(1,n) = 1 + (n-1)*5
                vu(2,n) = 2 + (n-1)*5
                vu(3,n) = 0
                vp(n) = 3 + (n-1)*5
                vpi(1,n) = 4 + (n-1)*5
                vpi(2,n) = 5 + (n-1)*5
                vpi(3,n) = 0
            end do
            goto 100
        endif
    endif
!
    call utmess('F', 'DVP_4', sk=nomte)
100  continue
!
    if (typmod(1) .eq. 'AXIS') then
        do n = 1, nno1
            vu(3,n) = vu(1,n)
        end do
    endif
!
end subroutine
