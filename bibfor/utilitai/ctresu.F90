subroutine ctresu(nomtb)
    implicit   none
#include "asterfort/ctacce.h"
#include "asterfort/ctcrtb.h"
#include "asterfort/ctdata.h"
#include "asterfort/cteltb.h"
#include "asterfort/ctnotb.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
    character(len=8) :: nomtb
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
!     ----- OPERATEUR CREA_TABLE , MOT-CLE FACTEUR RESU   --------------
!
!        BUT : CREER UNE TABLE A PARTIR D'UN RESULTAT OU D'UN CHAMP
!
!        IN/OUT : NOMTB (K8) : NOM DE LA TABLE
!
! ----------------------------------------------------------------------
    integer :: nbcmp, ndim, nbno, nbma, nbval
    logical(kind=1) :: toucmp
    character(len=1) :: tygd
    character(len=4) :: tych
    character(len=8) :: typac, sdres, noma
    character(len=16) :: nsymb
    character(len=19) :: chpgs
    character(len=24) :: nival, nrval, niord, nkcha, nkcmp, mesnoe, mesmai
!     ------------------------------------------------------------------
    call jemarq()
!
!  -- 1.INITIALISATION
!     -----------------
    chpgs = '&&CTRESU.PT_GAUSS_S'
    nival = '&&CTRESU.ACCES_IS'
    nrval = '&&CTRESU.ACCES_R8'
    nkcha = '&&CTRESU.SD_CHAM'
    niord = '&&CTRESU.ORDRE'
    nkcmp = '&&CTRESU.CMP_USER'
    mesmai = '&&CTRESU.MES_MAILLES'
    mesnoe = '&&CTRESU.MES_NOEUDS'
!
!  -- 2.RECUPERATIONS DES CHAMPS
!     --------------------------
    call ctacce(nsymb, typac, nbval, nival, nrval,&
                niord, nkcha, sdres)
!
!  -- 3.RECUPERATION DES NOEUDS,MAILLES,COMPOSANTES
!     ---------------------------------------------
    call ctdata(mesnoe, mesmai, nkcha, tych, toucmp,&
                nkcmp, nbcmp, ndim, chpgs, noma,&
                nbno, nbma, nbval, tygd)
!
!  -- 4.CREATION DE LA TABLE
!     ----------------------
    call ctcrtb(nomtb, tych, sdres, nkcha, typac,&
                toucmp, nbcmp, nbval, nkcmp, ndim)
!
!  -- 5.REMPLISSAGE DE LA TABLE
!     ----------------------
    if (tych .eq. 'NOEU') then
!
        call ctnotb(nbno, mesnoe, noma, nbval, nkcha,&
                    nkcmp, toucmp, nbcmp, typac, ndim,&
                    nrval, sdres, nomtb, nsymb, nival,&
                    niord)
!
    else if (tych(1:2).eq.'EL'.or.tych.eq.'CART') then
!
        call cteltb(nbma, mesmai, noma, nbval, nkcha,&
                    nkcmp, toucmp, nbcmp, typac, ndim,&
                    nrval, sdres, nomtb, nsymb, chpgs,&
                    tych, nival, niord)
!
    endif
!
!  -- 6.NETTOYAGE
!     ------------
    call jedetr(chpgs)
    call jedetr(nival)
    call jedetr(nrval)
    call jedetr(nkcha)
    call jedetr(niord)
    call jedetr(nkcmp)
    call jedetr(mesmai)
    call jedetr(mesnoe)
!
    call jedema()
!
end subroutine
