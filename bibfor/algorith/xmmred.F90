subroutine xmmred(ndimg, depdel, depdes, lagcn, depcn,&
                  fcont, fconts, fctcn, ffrot, ffrots,&
                  ffrocn)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "jeveux.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnsred.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: ndimg
    character(len=19) :: depdel, depdes, lagcn, depcn
    character(len=19) :: fcont, fconts, fctcn
    character(len=19) :: ffrot, ffrots, ffrocn
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEM - POST-TRAITEMENT)
!
! REDUCTION DU CHAMP SUR LES DDL
!
! ----------------------------------------------------------------------
!
!
! IN  NDIMG  : DIMENSION DE L'ESPACE
!
!
!
!
    character(len=8) :: ddlc(3), ddls2(4), ddls3(6), ddlt2(6), ddlt3(9)
! ----------------------------------------------------------------------
    data         ddlc  /'LAGS_C','LAGS_F1','LAGS_F2'/
    data         ddls2 /'H1X','H1Y','K1','K2'/
    data         ddls3 /'H1X','H1Y','H1Z','K1','K2','K3'/
    data         ddlt2 /'DX','DY','H1X','H1Y','K1','K2'/
    data         ddlt3 /'DX','DY','DZ','H1X','H1Y','H1Z',&
     &                    'K1','K2','K3'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- TRANSFORMATION DES CHAM_NO EN CHAM_NO_S
!
    call cnocns(depdel, 'V', depdes)
    call cnocns(fcont, 'V', fconts)
    call cnocns(ffrot, 'V', ffrots)
!
! --- REDUCTION SUR LES DDLS DE CONTACT
!
    call cnsred(depdes, 0, [0], ndimg, ddlc,&
                'V', lagcn)
!
! --- REDUCTION SUR LES DDLS DE SAUTS
!
    if (ndimg .eq. 3) call cnsred(depdes, 0, [0], 2*ndimg, ddls3,&
                                  'V', depcn)
    if (ndimg .eq. 2) call cnsred(depdes, 0, [0], 2*ndimg, ddls2,&
                                  'V', depcn)
!
! --- REDUCTION FORCE DE CONTACT
!
    if (ndimg .eq. 2) call cnsred(fconts, 0, [0], 3*ndimg, ddlt2,&
                                  'V', fctcn)
    if (ndimg .eq. 3) call cnsred(fconts, 0, [0], 3*ndimg, ddlt3,&
                                  'V', fctcn)
!
! --- REDUCTION FORCE DE FROTTEMENT
!
    if (ndimg .eq. 2) call cnsred(ffrots, 0, [0], 3*ndimg, ddlt2,&
                                  'V', ffrocn)
    if (ndimg .eq. 3) call cnsred(ffrots, 0, [0], 3*ndimg, ddlt3,&
                                  'V', ffrocn)
!
    call jedema()
end subroutine
