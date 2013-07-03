subroutine cesvar(carele, compor, ligrel, dcel)
    implicit none
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/detrsd.h"
#include "asterfort/jeexin.h"
    character(len=*) :: carele, compor, ligrel, dcel
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
!  BUT : CREER UN CHAM_ELEM_S (DCEL_I) PERMETTANT D'ETENDRE LES
!        CHAM_ELEM(VARI_R) "OUT" DE CALCUL.
!
!  IN/JXIN  CARELE K19 : CARA_ELEM  (OU ' ')
!                        C'EST CARA_ELEM QUI PERMET LES SOUS-POINTS
!  IN/JXIN  COMPOR K19 : / CARTE DE COMPORTEMENT
!                        / ' '
!  IN/JXIN  LIGREL K19 : LIGREL SUR LEQUEL SERA FAIT LE CALCUL
!  IN/JXOUT DCEL   K19 : CHAM_ELEM_S(DCEL_I) CONTENANT LE NOMBRE
!                        REEL DE VARIABLES INTERNES (ET DE SOUS-POINT)
!                        (MAILLE PAR MAILLE)
!  REMARQUES :
!   1) CETTE SD EST NECESSAIRE POUR ALLOUER "AU PLUS PRES" LES CHAM_ELEM
!      "DYNAMIQUES" : SOUS-POINTS OU  VARIABLES INTERNES (VARI_R)
!   2) SI LE CARA_ELEM N'A PAS DE SOUS-POINTS ET QUE COMPOR EST ' '
!      DCEL EST QUAND MEME CREE : IL CONTIENT NBSP=1, NBCDYN=0
! ----------------------------------------------------------------------
    character(len=8) :: carel2, lpain(2)
    character(len=19) :: lchin(2)
    integer :: nbch, iret
!-----------------------------------------------------------------------
    carel2 = carele
!
    nbch=0
    if (compor .ne. ' ') then
        nbch=nbch+1
        lpain(nbch) = 'PCOMPOR'
        lchin(nbch) = compor
    endif
!
!
    call jeexin(carel2//'.CANBSP    .CELD', iret)
    if (iret .gt. 0) then
        nbch=nbch+1
        lpain(nbch) = 'PNBSP_I'
        lchin(nbch) = carel2//'.CANBSP'
    endif
!
!
    call calcul('S', 'NSPG_NBVA', ligrel, nbch, lchin,&
                lpain, 1, '&&CESVAR.DCEL', 'PDCEL_I', 'V',&
                'NON')
!
    call celces('&&CESVAR.DCEL', 'V', dcel)
    call detrsd('CHAM_ELEM', '&&CESVAR.DCEL')
!
end subroutine
