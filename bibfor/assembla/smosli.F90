subroutine smosli(stomoz, stolcz, basz, rtbloc)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    character(len=*) :: stomoz, stolcz, basz
    real(kind=8) :: rtbloc
!     ------------------------------------------------------------------
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
!     CALCUL D'UN STOC_LCIEL A PARTIR D'UN STOC_MORSE (POUR CONTENIR
!     LA MEME MATRICE)
!     ------------------------------------------------------------------
! IN  JXIN  K19 STOMOZ     : NOM D'UNE S.D. STOC_MORSE
! IN  JXOUT K19 STOLCZ     : NOM D'UNE S.D. STOC_LCIEL
! IN        K1  BASZ       : BASE DE CREATION POUR STOLCZ
! IN        R   RTBLOC     : TAILLE DES BLOCS DE STOLCI
!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!     VARIABLES LOCALES
    character(len=1) :: base
    character(len=19) :: stomor, stolci
    integer ::  jscde, neq, nbloc
    integer :: jsmhc,  jscdi, jschc, jscbl, jscib, hcc
    integer :: hc, hcmax, itbloc, ieq, ibloc, tcumu, idiag, idiag1, imin, imax
    integer, pointer :: smde(:) => null()
    integer, pointer :: smdi(:) => null()
!     ------------------------------------------------------------------
!
!
!
    call jemarq()
    stomor=stomoz
    stolci=stolcz
    base=basz
!
!     -- ON DETRUIT STOLCI S'IL EXISTE DEJA :
    call detrsd('STOC_LCIEL', stolci)
!
!
!     -- OBJET .SCDE : C'EST FACILE MAIS INCOMPLET:
    call wkvect(stolci//'.SCDE', base//' V I', 6, jscde)
    call jeveuo(stomor//'.SMDE', 'L', vi=smde)
    neq=smde(1)
    zi(jscde-1+1)=neq
!
!     -- CALCUL DE ITBLOC :
    itbloc = nint(rtbloc*1024)
!
!
!     -- ALLOCATION DE  .SCHC .SCDI ET .SCIB :
    call wkvect(stolci//'.SCHC', base//' V I', neq, jschc)
    call wkvect(stolci//'.SCDI', base//' V I', neq, jscdi)
    call wkvect(stolci//'.SCIB', base//' V I', neq, jscib)
!
!
!     1. REMPLISSAGE DE .SCHC .SCDI ET .SCIB
!        CALCUL DE HCMAX, NBLOC:
!     -------------------------------------------------------------
    call jeveuo(stomor//'.SMDI', 'L', vi=smdi)
    call jeveuo(stomor//'.SMHC', 'L', jsmhc)
!
!     1.1  INITIALISATIONS :
    hcmax=0
    tcumu=0
    hcc=0
!
!     1.2  EQUATION 1 :
    nbloc=1
    hc=1
    hcc=hcc+hc
    zi(jschc-1+1)=hc
    zi(jscib-1+1)=nbloc
    zi(jscdi-1+1)=tcumu +hc
    hcmax=max(hcmax,hc)
    tcumu=tcumu+hc
!
!     1.3  EQUATIONS 2, ..., NEQ :
    do 1, ieq=2,neq
!        -- CALCUL DE HC : HAUTEUR DE LA COLONNE IEQ :
    idiag=smdi(ieq)
    idiag1=smdi(ieq-1)
    imin=zi4(jsmhc-1+ idiag1+1)
    imax=zi4(jsmhc-1+ idiag)
    hc=imax-imin+1
!
!        -- PEUT-ON ENCORE STOCKER CETTE COLONNE DANS LE BLOC COURANT ?
    ASSERT(hc.le.itbloc)
    if (tcumu+hc .gt. itbloc) then
        nbloc=nbloc+1
        tcumu=0
    endif
!
    zi(jschc-1+ieq)=hc
    zi(jscib-1+ieq)=nbloc
    zi(jscdi-1+ieq)=tcumu +hc
    hcmax=max(hcmax,hc)
    tcumu=tcumu+hc
    hcc=hcc+hc
    1 end do
    zi(jscde-1+3)=nbloc
    zi(jscde-1+4)=hcmax
!
!
!     2. ALLOCATION ET REMPLISSAGE DE .SCBL :
!     ----------------------------------------
    call wkvect(stolci//'.SCBL', base//' V I', nbloc+1, jscbl)
    zi (jscbl-1+1)=0
    ibloc=1
    do 2, ieq=1,neq
    if (zi(jscib-1+ieq) .gt. ibloc) then
        ibloc=ibloc+1
        zi (jscbl-1+ibloc)=ieq-1
    endif
    2 end do
    ASSERT(ibloc.eq.nbloc)
    zi (jscbl-1+nbloc+1)=neq
!
!
!     3. SI TOUTE LA MATRICE TIENT DANS UN SEUL BLOC, ON
!        ADAPTE LA TAILLE DE CE BLOC
!     ------------------------------------------------------
    if (itbloc .gt. hcc) then
        ASSERT(nbloc.eq.1)
        itbloc=hcc
    endif
    zi(jscde-1+2)=itbloc
!
!
    call jedema()
end subroutine
