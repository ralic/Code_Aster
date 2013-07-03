subroutine te0410(optioz, nomtz)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "jeveux.h"
!
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/tecach.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesi.h"
#include "asterfort/vdefro.h"
#include "asterfort/vdrepe.h"
#include "asterfort/vdsiro.h"
#include "asterfort/vdxedg.h"
#include "asterfort/vdxsig.h"
    character(len=*) :: optioz, nomtz
!
    character(len=16) :: option, nomte
!     ----------------------------------------------------------------
!     CALCUL DES OPTIONS DES ELEMENTS DE COQUE 3D
!                EPSI_ELGA
!                SIEF_ELGA
!                DEGE_ELGA
!                DEGE_ELNO
!
    integer :: npgt, ncoumx, vali(2)
!-----------------------------------------------------------------------
    integer :: i, icontr, jcou, jgeom, lzi
    integer :: nbcou, npgsn, npgsr
    real(kind=8) :: r8bid
!-----------------------------------------------------------------------
    parameter (npgt=10,ncoumx=10)
    integer :: nb1, itab(7), iret
    real(kind=8) :: effgt(8, 9), sigpg(162*ncoumx)
    real(kind=8) :: edgpg(72), defgt(72)
    real(kind=8) :: matevn(2, 2, npgt), matevg(2, 2, npgt)
! DEB
!
    option=optioz
    nomte=nomtz
!
    call jevech('PGEOMER', 'L', jgeom)
    call jevete('&INEL.'//nomte(1:8)//'.DESI', ' ', lzi)
    npgsn=zi(lzi-1+4)
!
    call jevech('PNBSP_I', 'L', jcou)
    nbcou=zi(jcou)
    vali(1)=ncoumx
    vali(2)=nbcou
    if (nbcou .gt. ncoumx) call u2mesi('F', 'CALCULEL7_4', 2, vali)
    if (option(1:9) .eq. '         ') then
        call u2mesg('F', 'CALCULEL7_5', 1, option, 1,&
                    nbcou, 0, r8bid)
    endif
!     LE TABLEAU SIGPG A ETE ALLOUE DE FACON STATIQUE POUR OPTIMISER
!     LE CPU CAR LES APPELS A WKVECT DANS LES TE SONT COUTEUX.
!
    if (option(1:9) .eq. 'DEGE_ELGA' .or. option(1:9) .eq. 'DEGE_ELNO') then
        call vdxedg(nomte, option, zr(jgeom), nb1, npgsr,&
                    edgpg, defgt)
    else
        call vdxsig(nomte, option, zr(jgeom), nb1, npgsr,&
                    sigpg, effgt)
    endif
!
! --- DETERMINATION DES MATRICES DE PASSAGE DES REPERES INTRINSEQUES
! --- AUX NOEUDS ET AUX POINTS D'INTEGRATION DE L'ELEMENT
! --- AU REPERE UTILISATEUR :
!     ---------------------
    call vdrepe(nomte, matevn, matevg)
    if (option(1:9) .eq. 'EPSI_ELGA') then
        call tecach('OOO', 'PDEFOPG', 'E', 7, itab,&
                    iret)
        icontr=itab(1)
!
! ----- STOCKAGE DU VECTEUR DES DEFORMATIONS
! ----- 1 COUCHE - 3 PTS DANS L'EPAISSEUR
!------ 162 = NPGSN*6(6 DEFORMATIONS STOCKEES)*NPGE
!       ------------------------------------------------------
!
        do 11 i = 1, npgsn*18*nbcou
            zr(icontr-1+i)=sigpg(i)
11      continue
!
! ---   PASSAGE DES DEFORMATIONS DANS LE REPERE UTILISATEUR :
        call vdsiro(itab(3), itab(7), matevg, 'IU', 'G',&
                    zr(icontr), zr( icontr))
!
!
    else if (option(1:9) .eq. 'SIEF_ELGA') then
        call tecach('OOO', 'PCONTRR', 'E', 7, itab,&
                    iret)
        icontr=itab(1)
!
! ----- STOCKAGE DU VECTEUR DES CONTRAINTES EN ELASTICITE
! ----- 1 COUCHE - 3 PTS DANS L'EPAISSEUR
!------ 162 = NPGSN*6(6 CONTRAINTES STOCKEES)*NPGE
!       ------------------------------------------------------
!
        do 10 i = 1, npgsn*18*nbcou
            zr(icontr-1+i)=sigpg(i)
10      continue
!
! ---   PASSAGE DES CONTRAINTES DANS LE REPERE UTILISATEUR :
        call vdsiro(itab(3), itab(7), matevg, 'IU', 'G',&
                    zr(icontr), zr( icontr))
!
!
    else if (option(1:9) .eq. 'DEGE_ELGA') then
        call tecach('OOO', 'PDEFOPG', 'E', 7, itab,&
                    iret)
        icontr=itab(1)
!
! ---   PASSAGE DES DEFORMATIONS DANS LE REPERE UTILISATEUR
!       ET STOCKAGE DES DEFORMATIONS:
!
        call vdefro(npgsn, matevn, edgpg, zr(icontr))
!
    else if (option(1:9) .eq. 'DEGE_ELNO') then
        call tecach('OOO', 'PDEFOGR', 'E', 7, itab,&
                    iret)
        icontr=itab(1)
!
! ---   PASSAGE DES DEFORMATIONS DANS LE REPERE UTILISATEUR
!       ET STOCKAGE DES DEFORMATIONS:
!
        call vdefro((nb1+1), matevn, defgt, zr(icontr))
!
    endif
!
end subroutine
