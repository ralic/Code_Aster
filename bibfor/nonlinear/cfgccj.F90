subroutine cfgccj(resoco, nbliai, conjug)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "blas/daxpy.h"
#include "blas/ddot.h"
#include "blas/dscal.h"
    character(len=24) :: resoco
    integer :: nbliai
    logical(kind=1) :: conjug
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (RESOLUTION - GCP)
!
! CONJUGAISON
!
! ----------------------------------------------------------------------
!
! ON FAIT LA CONJUGAISON DE POLAK-RIBIERE :
!     - SI L'ETAT DE CONTACT EST LE MEME D'UNE ITERATION SUR L'AUTRE
!     - TOUTES LES FREQ ITERATIONS
!     - SI DIRECT EST UNE DIRECTION DE DESCENTE I.E.
!                                                  (DIRECT' SGRAD+)>0
!  NB1 :  FORMULE DE CONJUGAISON EN PRESENCE DE PRECONDITIONNEUR :
!         GAMMA = (SGRADP' (SGRPRP - SGRPRM)) / (SGRADM' SGRPRM))
!  NB2 : LA CONJUGAISON DE FLETCHER-REEVES EST : GAMMA = NUMER/DENOM
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NBLIAI : NOMBRE DE LIAISONS DE CONTACT
! I/O CONJUG : DIRECTIONS CONJUGUEES
!
!
!
!
!
    integer :: ifm, niv
    real(kind=8) :: numer, numer2, denom, gamma
    character(len=19) :: sgradm, sgradp, sgrprm, sgrprp, direct
    integer :: jsgram, jsgrap, jsgprm, jsgprp, jdirec
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- INITIALISATIONS
!
    gamma = 0.d0
!
! --- ACCES VECTEURS DE TRAVAIL
!
    sgradm = resoco(1:14)//'.SGDM'
    sgradp = resoco(1:14)//'.SGDP'
    sgrprm = resoco(1:14)//'.SGPM'
    sgrprp = resoco(1:14)//'.SGPP'
    direct = resoco(1:14)//'.DIRE'
    call jeveuo(sgradm, 'L', jsgram)
    call jeveuo(sgradp, 'L', jsgrap)
    call jeveuo(sgrprm, 'L', jsgprm)
    call jeveuo(sgrprp, 'L', jsgprp)
    call jeveuo(direct, 'E', jdirec)
!
! --- COEFFICIENT DE DIRECTION
!
    if (conjug) then
        numer = ddot(nbliai,zr(jsgrap),1,zr(jsgprp),1)
        numer2 = ddot(nbliai,zr(jsgrap),1,zr(jsgprm),1)
        denom = ddot(nbliai,zr(jsgram),1,zr(jsgprm),1)
        gamma = (numer-numer2)/denom
    endif
!
! --- MISE A JOUR DIRECTION
!
    call dscal(nbliai, gamma, zr(jdirec), 1)
    call daxpy(nbliai, 1.d0, zr(jsgprp), 1, zr(jdirec),&
               1)
!
! --- AFFICHAGE
!
    if (conjug) then
        if (niv .eq. 2) then
            write (ifm,*) '<CONTACT><CALC> CONJUGAISON DES DIRECTIONS '//&
     &      'DE RECHERCHE, GAMMA=',gamma
        endif
    endif
!
    call jedema()
!
end subroutine
