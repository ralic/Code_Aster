subroutine cccmcr(jcesdd, numma, jrepe, jconx2, jconx1,&
                  jcoord, adcar1, adcar2, ialpha, ibeta,&
                  iepais, jalpha, jbeta, jgamma, ligrmo,&
                  ino, pgl, modeli, codret)
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
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/c3drep.h"
#include "asterfort/cesexi.h"
#include "asterfort/dismoi.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/matrot.h"
#include "asterfort/mpglcp.h"
#include "asterfort/typele.h"
    integer :: jcesdd, numma, jrepe, jconx2, jconx1, jcoord
    integer :: ialpha, ibeta, iepais, jalpha, adcar1(3), adcar2(3)
    integer :: jbeta, jgamma, codret, ino
    real(kind=8) :: pgl(3, 3)
    character(len=16) :: modeli
    character(len=19) :: ligrmo
!     --- ARGUMENTS ---
! ----------------------------------------------------------------------
!  CALC_CHAMP - CALCUL DE LA MATRICE DE CHANGEMENT DE REPERE
!  -    -       -            -          -             -
! ----------------------------------------------------------------------
!
!  ROUTINE SERVANT A CALCULER LA MATRICE PGL
!
! IN  :
!   BEAUCOUP DE PARAMETRES UTILES POUR EVITER DES APPELS DEJA REALISES
!   DANS L'APPELANT CCVRRL
!
! OUT :
!   PGL     R*   MATRICE DE PASSAGE DU REPERE GLOBAL AU REPERE LOCAL
!   MODELI  K16  NOM DE LA MODELISATION, CETTE INFO EST UTILE A
!                L'APPELANT
!   CODRET  I    CODE RETOUR
!     0 SI OK
!     1,2 OU 3 EN CAS DE PROBLEME
! ----------------------------------------------------------------------
! person_in_charge: nicolas.sellenet at edf.fr
    integer :: nbpt1, igrel, te, ibid, ier, nbnol, posin, ino1, ino2, idir
    integer :: iad, inos, nuno, jcesd, jcesl, jcesv, jcesdc, jceslc
    integer :: jcesvc
!
    real(kind=8) :: coordc(3, 10), alpha, beta, gamma, epais, ang1(3)
!
    character(len=16) :: nomte
!
    codret = 3
!
    jcesd = adcar1(1)
    jcesl = adcar1(2)
    jcesv = adcar1(3)
    jcesdc = adcar2(1)
    jceslc = adcar2(2)
    jcesvc = adcar2(3)
!
!     SI LE CHAMP A TRANSFORMER NE PORTE PAS DE COMPOSANTES
!     SUR LA MAILLE ON DOIT LE PASSER
    nbpt1 = zi(jcesdd-1+5+4*(numma-1)+3)
    if (nbpt1 .eq. 0) goto 9999
    codret = 0
!
!     RECUPERATION DE LA MODELISATION
    igrel = zi(jrepe-1+2*(numma-1)+1)
    te = typele(ligrmo,igrel)
    call jenuno(jexnum('&CATA.TE.NOMTE', te), nomte)
    call dismoi('F', 'MODELISATION', nomte, 'TYPE_ELEM', ibid,&
                modeli, ier)
!
    nbnol = zi(jconx2+numma)-zi(jconx2+numma-1)
    posin = zi(jconx2+numma-1)
!
!     SUIVANT LE TYPE DE MODELISATION, LE CHANGEMENT DE REPERE
!     N'EST PAS LE MEME BARRE, CABLE, TUYAU
    if ((modeli(1:3).eq.'POU') .or. (modeli(1:7).eq.'2D_DIS_') .or. (modeli.eq.'BARRE')&
        .or. (modeli.eq.'CABLE') .or. (modeli.eq.'TUYAU')) then
!
        ino1 = zi(jconx1+posin-1)
        ino2 = zi(jconx1+posin)
        do 70 idir = 1, 3
            coordc(idir,1) = zr(jcoord+3*(ino2-1)+idir-1)
70      continue
        do 80 idir = 1, 3
            coordc(idir,2) = zr(jcoord+3*(ino1-1)+idir-1)
80      continue
!
!       LECTURE DE GAMMA DANS .CARORIEN
        call cesexi('S', jcesd, jcesl, numma, 1,&
                    1, 3, iad)
        if (iad .gt. 0) then
            gamma = zr(jcesv-1+iad)
        else
            call assert(.false.)
        endif
!
        call mpglcp('P', nbnol, coordc, alpha, beta,&
                    gamma, pgl, codret)
!
    else if ((modeli.eq.'COQUE_3D')) then
!
        if (nbnol .lt. 7) then
            codret = 1
            goto 9999
        endif
        inos = 0
        do 160 ino2 = 1, nbnol
            nuno = zi(jconx1+posin+ino2-2)
            if (nuno .eq. ino) inos = ino2
            do 150 idir = 1, 3
                coordc(idir,ino2) = zr(jcoord+3*(nuno-1)+idir-1)
150          continue
160      continue
        call assert(inos.ne.0)
!
!       RECHERCHE DE ALPHA ET BETA DANS .CARCOQUE
        call cesexi('S', jcesdc, jceslc, numma, 1,&
                    1, ialpha, iad)
        alpha = zr(jcesvc-1+iad)
        call cesexi('S', jcesdc, jceslc, numma, 1,&
                    1, ibeta, iad)
        beta = zr(jcesvc-1+iad)
        call cesexi('S', jcesdc, jceslc, numma, 1,&
                    1, iepais, iad)
        epais = zr(jcesvc-1+iad)
!
        call c3drep(nomte, epais, alpha, beta, coordc,&
                    inos, pgl)
!
        elseif ( (modeli(1:3).eq.'DKT') .or.(modeli(1:3).eq.'DST')&
    .or.(modeli(1:4).eq.'DKTG') .or.(modeli(1:3).eq.'Q4G') .or.(&
    modeli.eq.'COQUE') .or.(modeli.eq.'GRILLE') ) then
!
        do 50 ino2 = 1, nbnol
            nuno = zi(jconx1+posin+ino2-2)
            do 60 idir = 1, 3
                coordc(idir,ino2) = zr(jcoord+3*(nuno-1)+idir-1)
60          continue
50      continue
!
!       RECHERCHE DE ALPHA ET BETA DANS .CARCOQUE
        call cesexi('S', jcesdc, jceslc, numma, 1,&
                    1, ialpha, iad)
        alpha = zr(jcesvc-1+iad)
        call cesexi('S', jcesdc, jceslc, numma, 1,&
                    1, ibeta, iad)
        beta = zr(jcesvc-1+iad)
!
        call mpglcp('C', nbnol, coordc, alpha, beta,&
                    gamma, pgl, codret)
!
    else
!
!       SI ON EST NI DANS LE CAS D'UNE POUTRE OU D'UNE COQUE
!       C'EST LE CAS GENERAL : CARORIEN
        if (jcesd .ne. 0) then
            call cesexi('S', jcesd, jcesl, numma, 1,&
                        1, jalpha, iad)
            ang1(1) = zr(jcesv-1+iad)
            call cesexi('S', jcesd, jcesl, numma, 1,&
                        1, jbeta, iad)
            ang1(2) = zr(jcesv-1+iad)
            call cesexi('S', jcesd, jcesl, numma, 1,&
                        1, jgamma, iad)
            ang1(3) = zr(jcesv-1+iad)
        else
            ang1(1) = 0.d0
            ang1(2) = 0.d0
            ang1(3) = 0.d0
        endif
        call matrot(ang1, pgl)
!
    endif
!
9999  continue
!
end subroutine
