subroutine intfac(noma, nmaabs, ifq, fa, nno,&
                  lst, lsn, ndim, grad, jglsn,&
                  jglst, igeom, m, indptf, gln,&
                  glt, codret)
! aslint: disable=W1306
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/elrfvf.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/reereg.h"
#include "asterfort/vecini.h"
    integer :: ifq, fa(6, 8), nno, ndim, jglsn, jglst, igeom, codret
    integer :: indptf(3), nmaabs
    real(kind=8) :: lsn(nno), lst(nno), m(ndim), gln(ndim), glt(ndim)
    character(len=3) :: grad
    character(len=8) :: noma
!
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
! person_in_charge: samuel.geniaut at edf.fr
!              TROUVER LES PTS D'INTERSECTION ENTRE LE FOND DE FISSURE
!                 ET UNE FACE POUR LES ELEMENTS EN FOND DE FISSURE
!
!     ENTREE
!  NOMA   : NOM DU MAILLAGE
!  NMAABS : NUMERO DE LA MAILLE
!  IFQ    : NUMERO LOCAL DE LA FACE DE LA MAILLE
!  FA     : COONECTIVITE DES FACES DE LA MAILLE
!  NNO    : NOMBRE DE NOEUDS DE LA MAILLE
!  LSN    : VECTEUR LOCAL DES VALEURS NODALES DE LA MAILLE POUR LSN
!  LST    : VECTEUR LOCAL DES VALEURS NODALES DE LA MAILLE POUR LST
!  NDIM   : DIMENSION DE L'ESPACE
!  GRAD   : SI 'OUI' : ON CALCULE AUSSI LES GRADIENTS AU POINT TROUVE
!  JGLSN  : ADRESSE DU VECTEUR LOCAL DES VALEURS NODALES DE GRAD DE LSN
!  JGLST  : ADRESSE DU VECTEUR LOCAL DES VALEURS NODALES DE GRAD DE LST
!  IGEOM  : ADRESSE DU VECTEUR LOCAL DES COORDONNEES DES NOEUDS
!
!     SORTIE
!  M      : POINT TROUVE
!  INDPTF : VECTEUR INDICE DU POINT M TROUVE
!  GLN    : GRAD DE LSN EN M (SI DEMANDE)
!  GLT    : GRAD DE LST EN M (SI DEMANDE)
!  CODRET : CODE RETOUR = 1 SI ON A BIEN TROUVE UN POINT M
!                       = 0 SI ON N'A PAS PU TROUVE UN UNIQUE POINT M
!
!     ------------------------------------------------------------------
!
    integer :: nnof, i, j, k, nne, ino, iret, jconx2, numnoa, numnob
    real(kind=8) :: coorma(8), prec, mp(2), epsi(2), ff(nno), lsta, lsna, lstb
    real(kind=8) :: lsnb, solsn, a(ndim), b(ndim), mem(3), memo, normab, coeffk
    real(kind=8) :: prec2, length(12)
    character(len=8) :: alias
    aster_logical :: chgsgn
    integer, pointer :: connex(:) => null()
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call vecini(ndim, 0.d0, m)
    if (grad .eq. 'OUI') then
        call vecini(ndim, 0.d0, gln)
        call vecini(ndim, 0.d0, glt)
    endif
    do 100 i = 1, 3
        indptf(i)=0
100 continue
!
    prec=r8prem()
    prec2= 1.d-4
    codret = 0
!
!     INITIALISATION DES COORDONNéES (LS) DES NOEUDS DE LA FACE
    call vecini(8, 0.d0, coorma)
    lsta=0.d0
    lsna=0.d0
    lstb=0.d0
    lsnb=0.d0
!
!     RECUPERATION DES DONNEES SUR LE MAILLAGE
    call jeveuo(noma//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
!     NOMBRE DE SOMMETS DE LA FACE
    if (fa(ifq,4) .eq. 0) then
        nnof = 3
        alias = 'TR3'
    else
        nnof = 4
        alias='QU4'
    endif
    ASSERT(nnof.le.4)
!
!     NOEUDS SOMMETS DE LA FACE : FA(IFQ,1) ... FA(IFQ,NNOF)
!
    chgsgn = .false.
!
!     SI LA FACE COINCIDE AVEC LA SURFACE DE LA FISSURE, ON SORT
!     (CAD SI LES LSN DES SOMMETS DE LA FACE SONT TOUS NULS)
    solsn = 0.d0
    do 200 i = 1, nnof
        solsn = solsn + abs( lsn(fa(ifq,i)) )
200 continue
    if (solsn .eq. 0.d0) goto 999
!
    do 220 i = 1, nnof
        if (i .eq. 1) then
            j = nnof
        else
            j = i-1
        endif
        lsta=lst(fa(ifq,i))
        lsna=lsn(fa(ifq,i))
        lstb=lst(fa(ifq,j))
        lsnb=lsn(fa(ifq,j))
        coorma(2*i-1)=lsta
        coorma(2*i)=lsna
!
!       SI LE FOND COINCIDE AVEC UN COTE DE LA FACE, ON SORT
        if (lsna .eq. 0.d0 .and. lsnb .eq. 0.d0 .and. lsta .eq. 0.d0 .and. lstb .eq. 0.d0) &
        goto 999
!
!       ON ACCEPTE TOUT DE SUITE LA FACE SI LE FRONT COINCIDE
!       AVEC L'UN DES NOEUDS DE LA FACE
        if ((lsna.eq.0.d0) .and. (lsta.eq.0.d0)) then
            chgsgn = .true.
            indptf(1)=1
            indptf(2)=connex(zi(jconx2+nmaabs-1)+fa(ifq,i)-1)
            goto 220
!
        endif
!
!       ON ACCEPTE TOUT DE SUITE LA FACE SI LE FRONT COINCIDE
!       AVEC UN POINT D'UNE ARETE DE LA FACE
        if (((lsna.eq.0.d0).and.(lsnb.eq.0.d0)) .and. ((lsta*lstb) .lt.r8prem())) then
            chgsgn = .true.
            indptf(1)=2
            indptf(2)=connex(zi(jconx2+nmaabs-1)+fa(ifq,i)-1)
            indptf(3)=connex(zi(jconx2+nmaabs-1)+fa(ifq,j)-1)
            goto 220
!
        endif
!
!       ON NE CONSERVE QUE LES FACES COUPEES
!       SUR UNE SEULE ARETE PAR LE DEMI-PLAN (LSN=0/LST<0)
!       POUR CELA, ON CONTROLE LE SIGNE DE LST(I) OU I EST LE POINT
!       D'INTERSECTION DE L'ARETE AVEC LSN=0
!       (ON A LSN(I)=LST(B)-LSN(B)*(LST(A)-LST(B))/(LSN(A)-LSN(B)))
        if ((lsna*lsnb) .lt. prec) then
!
            if ((&
                (abs((lsna-lsnb)).gt.r8prem()) .and.&
                ((lstb-(lsnb*( lsta-lstb)/(lsna-lsnb))).lt.prec)&
                )&
                .or. ( (abs((lsna-lsnb)) .le.r8prem()) .and. ((lsta*lstb).lt.r8prem()) )) then
!
                chgsgn = .true.
                indptf(1)=3
                indptf(2)=0
                indptf(3)=0
!
            endif
!
        endif
!
220 continue
!
    if (.not. chgsgn) goto 999
!
!     ON CHERCHE SUR LA MAILLE LE POINT CORRESPONDANT à LSN=LST=0
    mp(1)=0.d0
    mp(2)=0.d0
    call reereg('C', alias, nnof, coorma, mp,&
                2, epsi, iret)
!
    if (iret .eq. 1) goto 999
!
!     ON NE PREND PAS EN COMPTE LES POINTS QUI SORTENT DU DOMAINE
!     ON AJOUTE UN PETIT PREC ICI POUR RAISON DE PRECISION DANS
!     LA COMPARAISON, CF. LA FICHE 20170.
!     -> PREC*1.d3 POUR ISSUE21167
!     -> PREC*1.d4 POUR ISSUE22492
    if (alias .eq. 'QU4') then
        if (abs(epsi(1)) .gt. (1.d0+prec*1.d4)) goto 999
        if (abs(epsi(2)) .gt. (1.d0+prec*1.d4)) goto 999
    else if (alias.eq.'TR3') then
        if (epsi(1) .lt. (0.d0-prec*1.d4)) goto 999
        if (epsi(2) .lt. (0.d0-prec*1.d4)) goto 999
        if (epsi(1)+epsi(2) .gt. (1.d0+prec*1.d4)) goto 999
    endif
!
    mp(1)=epsi(1)
    mp(2)=epsi(2)
!     ON DOIT MAINTENANT MULTIPLIER LES COORD. PARAM. DE M PAR CHACUNE
!     DES FF DES NOEUDS DE L'éLéMENT POUR OBTENIR LES COORD. CART.
    call elrfvf(alias, mp, nnof, ff, nne)
    do 230 i = 1, ndim
        do 240 j = 1, nnof
            ino = fa(ifq,j)
            m(i) = m(i) + zr(igeom-1+ndim*(ino-1)+i) * ff(j)
            if (grad .eq. 'OUI') then
                glt(i) = glt(i) + zr(jglst-1+ndim*(ino-1)+i) * ff(j)
                gln(i) = gln(i) + zr(jglsn-1+ndim*(ino-1)+i) * ff(j)
            endif
240     continue
230 continue
!
!     TRAITEMENT DES POINTS M PROCHES DES SOMMETS (FIT TO VERTEX)
    if ((indptf(1).eq.2) .or. (indptf(1).eq.3)) then
        do 555 i = 1, nnof
            memo=0.d0
            do 556 j = 1, ndim
                a(j)=zr(igeom-1+ndim*(fa(ifq,i)-1)+j)
                memo=memo+(a(j)-m(j))**2
556         continue
            length(3*(i-1)+1)=sqrt(memo)
            length(3*(i-1)+2)= connex(zi(jconx2+nmaabs-1)+fa(ifq,&
            i)-1)
            length(3*(i-1)+3)= 0
555     continue
!       ON TRIE LE VECTEUR LENGTH
        do 655 i = 1, nnof-1
            do 755 j = i+1, nnof
                if (length(3*(j-1)+1) .lt. length(3*(i-1)+1)) then
                    do 756 k = 1, 3
                        mem(k) = length(3*(i-1)+k)
756                 continue
                    do 757 k = 1, 3
                        length(3*(i-1)+k) = length(3*(j-1)+k)
757                 continue
                    do 758 k = 1, 3
                        length(3*(j-1)+k) = mem(k)
758                 continue
                endif
755         continue
655     continue
!       M EST PROCHE D'UN SOMMET ? SI OUI, ON LE REPLACE SUR LE SOMMET
        if (length(1) .lt. (prec2*length(4))) then
            indptf(1)= 1
            indptf(2)= int(length(2))
            indptf(3)= 0
            goto 222
        endif
    endif
!
!     TRAITEMENT DES POINTS M PROCHES DES ARETES (FIT TO VERTEX)
    if (indptf(1) .eq. 3) then
        do 955 i = 1, nnof
            do 105 j = 1, ndim
                a(j)=zr(igeom-1+ndim*(fa(ifq,i)-1)+j)
105         continue
            numnoa=connex(zi(jconx2+nmaabs-1)+fa(ifq,i)-1)
            if (i .eq. nnof) then
                do 106 j = 1, ndim
                    b(j)=zr(igeom-1+ndim*(fa(ifq,1)-1)+j)
106             continue
                numnob=connex(zi(jconx2+nmaabs-1)+fa(ifq,1)-1)
            else
                do 107 j = 1, ndim
                    b(j)=zr(igeom-1+ndim*(fa(ifq,i+1)-1)+j)
107             continue
                numnob=connex(zi(jconx2+nmaabs-1)+fa(ifq,i+1)-1)
            endif
            normab=0.d0
            coeffk=0.d0
            memo=0.d0
            do 108 k = 1, ndim
                normab=normab+(b(k)-a(k))**2
                coeffk=coeffk+(b(k)-a(k))*(m(k)-a(k))
108         continue
            do 109 k = 1, ndim
                memo=memo+ (a(k)-m(k)+(coeffk/normab)*(b(k)-a(k)))**2
109         continue
            length(3*(i-1)+1)= memo
            length(3*(i-1)+2)= numnoa
            length(3*(i-1)+3)= numnob
955     continue
!       ON TRIE LE VECTEUR LENGTH
        do 958 i = 1, nnof-1
            do 959 j = i+1, nnof
                if (length(3*(j-1)+1) .lt. length(3*(i-1)+1)) then
                    do 960 k = 1, 3
                        mem(k) = length(3*(i-1)+k)
960                 continue
                    do 961 k = 1, 3
                        length(3*(i-1)+k) = length(3*(j-1)+k)
961                 continue
                    do 962 k = 1, 3
                        length(3*(j-1)+k) = mem(k)
962                 continue
                endif
959         continue
958     continue
!       M EST PROCHE D'UNE ARETE ? SI OUI, ON LE REPLACE SUR L'ARETE
        if (length(1) .lt. (prec2*length(4))) then
            indptf(1) = 2
            indptf(2) = int(length(2))
            indptf(3) = int(length(3))
            goto 222
        endif
    endif
!
222 continue
!
!     TOUT S'EST BIEN PASSE
    codret = 1
!
999 continue
!
    call jedema()
end subroutine
