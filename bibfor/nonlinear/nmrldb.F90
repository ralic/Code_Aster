subroutine nmrldb(solveu, lmat, resu, nbsm, cncine)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    implicit none
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/resoud.h"
    integer :: lmat, nbsm
    real(kind=8) :: resu(*)
    character(len=19) :: solveu, cncine
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT
!
! ROUTINE DE CALCUL DE RESU = MAT-1(RESU,CNCINE)  NON LINEAIRE
!
! ----------------------------------------------------------------------
!
!
! IN  SOLVEU : SD SOLVEUR
! IN  LMAT   : DESCRIPTEUR DE LA MATR_ASSE
! IN  CNCINE : NOM DU CHARGEMENT CINEMATIQUE
! I/O RESU   : .VALE DU CHAM_NO RESULTAT EN OUT , SMB EN IN
!
!
!
! ----------------------------------------------------------------------
!
    character(len=19) :: matr
    complex(kind=8) :: c16bid
    integer :: iret
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    matr = zk24(zi(lmat+1))
!
    call resoud(matr, ' ', solveu, cncine, nbsm,&
                ' ', ' ', 'V', resu, c16bid,&
                ' ', .true., 0, iret)
!
    call jedema()
!
end subroutine
