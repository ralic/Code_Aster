subroutine dilcar(option, icompo, icontm, ideplm, ideplp,&
                  igeom, imate, imatuu, ivectu, icontp,&
                  ivarip, ichg, ichn, jcret, idefo)
! ======================================================================
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
! ======================================================================
    implicit      none
#include "asterc/ismaem.h"
#include "asterfort/assert.h"
#include "asterfort/jevech.h"
    integer :: icompo, icontm, ideplm, ideplp, igeom, imate, jcret, idefo
    integer :: imatuu, ivectu, icontp, ichg, ichn, ivarip
    character(len=16) :: option
! ======================================================================
! --- BUT : RECUPERATION DES ADRESSES DES CHAMPS DE L'ELEMENT ----------
! -------   POUR LES MODELES SECOND GRADIENT ---------------------------
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: ivarim
! ======================================================================
! --- INITIALISATION DE TOUTES LES ADRESSES A L'ENTIER MAXIMAL ---------
! ======================================================================
    icompo=ismaem()
    icontm=ismaem()
    ideplm=ismaem()
    ideplp=ismaem()
    igeom =ismaem()
    imate =ismaem()
    ivarim=ismaem()
    imatuu=ismaem()
    ivectu=ismaem()
    icontp=ismaem()
    ivarip=ismaem()
    ichg  =ismaem()
    ichn  =ismaem()
    jcret =ismaem()
    idefo =ismaem()
! ======================================================================
    if (option .eq. 'CHAR_MECA_PESA_R') then
! OPTION NON PROGRAMMEE
        call assert(.false.)
    endif
! ======================================================================
! --- RECUPERATION DES CHAMPS D'ENTREE DE L'ELEMENT --------------------
! ======================================================================
    if (option(1:9) .eq. 'RIGI_MECA') then
        call jevech('PCOMPOR', 'L', icompo)
        call jevech('PCONTMR', 'L', icontm)
        call jevech('PDEPLMR', 'L', ideplm)
        call jevech('PDEPLPR', 'L', ideplp)
        call jevech('PGEOMER', 'L', igeom)
        call jevech('PMATERC', 'L', imate)
        call jevech('PVARIMR', 'L', ivarim)
        call jevech('PMATUNS', 'E', imatuu)
    else if (option.eq.'RAPH_MECA') then
        call jevech('PCOMPOR', 'L', icompo)
        call jevech('PCONTMR', 'L', icontm)
        call jevech('PDEPLMR', 'L', ideplm)
        call jevech('PDEPLPR', 'L', ideplp)
        call jevech('PGEOMER', 'L', igeom)
        call jevech('PMATERC', 'L', imate)
        call jevech('PVARIMR', 'L', ivarim)
        call jevech('PVECTUR', 'E', ivectu)
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
        call jevech('PCODRET', 'E', jcret)
    else if (option(1:9).eq.'FULL_MECA') then
        call jevech('PCOMPOR', 'L', icompo)
        call jevech('PCONTMR', 'L', icontm)
        call jevech('PDEPLMR', 'L', ideplm)
        call jevech('PDEPLPR', 'L', ideplp)
        call jevech('PGEOMER', 'L', igeom)
        call jevech('PMATERC', 'L', imate)
        call jevech('PVARIMR', 'L', ivarim)
        call jevech('PMATUNS', 'E', imatuu)
        call jevech('PVECTUR', 'E', ivectu)
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
        call jevech('PCODRET', 'E', jcret)
    else if (option.eq.'FORC_NODA') then
        call jevech('PCOMPOR', 'L', icompo)
        call jevech('PCONTMR', 'L', icontm)
        call jevech('PDEPLMR', 'L', ideplm)
        call jevech('PGEOMER', 'L', igeom)
        call jevech('PMATERC', 'L', imate)
        call jevech('PVECTUR', 'E', ivectu)
    else if (option.eq.'EPSI_ELGA') then
        call jevech('PGEOMER', 'L', igeom)
        call jevech('PDEPLAR', 'L', ideplp)
        call jevech('PDEFOPG', 'E', idefo)
    endif
! ======================================================================
end subroutine
