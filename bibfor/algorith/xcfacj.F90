subroutine xcfacj(ptint, ptmax, ipt, ainter, lsn,&
                  igeom, nno, ndim, nfiss, ifiss,&
                  fisco, nfisc, typma)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=W1306
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/confac.h"
#include "asterfort/elref1.h"
#include "asterfort/elrfvf.h"
#include "asterfort/padist.h"
#include "asterfort/reereg.h"
#include "asterfort/xajpin.h"
    integer :: ptmax, ipt, igeom, nno, ndim
    real(kind=8) :: lsn(*), ptint(*), ainter(*)
    integer :: nfiss, ifiss, fisco(*), nfisc
    character(len=8) :: typma
!
!              TROUVER LES PTS D'INTER ENTRE LES JONCTIONS DE FISSURE
!                 ET LES FACES POUR LES ELEMENTS MULTI-HEAVISIDE
!
!     ENTREE
!       PTINT   : COORDONNEES DES POINTS D'INTERSECTION
!       PTMAX    : NOMBRE MAX DE POINTS D'INTERSECTION
!       IPT      : COMPTEUR DE NOMBRE DE POINTS D'INTERSECTION
!       AINTER   : INFOS SUR LES ARETES ASSOCIEES
!       LSN      : VALEURS DE LA LEVEL SET NORMALE
!       IGEOM    : ADRESSE DES COORDONNEES DES NOEUDS DE L'ELT PARENT
!       NNO      : NOMBRE DE NOEUDS DE L'ELEMENT
!       NDIM     : DIMENSION DE L'ESPACE
!       NFISS    : NOMBRE DE FISSURES VUES DANS L'ÉLÉMENT
!       IFISS    : NUMÉRO DE LA FISSURE EN COURS
!       FISCO    : NUM ET COEF DES FISS SUR LESQUELLES IFISS SE BRANCHE
!       NFISC    : NOMBRE DE FISSURES SUR LESQUELLES IFISS SE BRANCHE
!       TYPMA    : TYPE DE LA MAILLE ASSOCIEE A L'ELEMENT
!
!     SORTIE
!       PTINT    : COORDONNEES DES POINTS D'INTERSECTION
!       IPT      : COMPTEUR DE NOMBRE DE POINTS D'INTERSECTION
!       AINTER   : INFOS SUR LES ARETES ASSOCIEES
!
!     ------------------------------------------------------------------
!
    character(len=8) :: elref, alias
    real(kind=8) :: lsna, lsnb, lsja, lsjb, lsj
    real(kind=8) :: a(3), b(3), c(3), mp(2), prec, ff(nno)
    real(kind=8) :: loncar, dst
    real(kind=8) :: m(3), somlsn, epsi(2), coorma(8)
    integer :: i, nbf, ibid, ifq, j
    integer :: fa(6, 8), ibid3(12, 3), ifisc, jfisc, ino
    integer :: nnof, na, nb, iret, nne
    aster_logical :: chgsgn, lajpf, ajout
! ----------------------------------------------------------------------
!
    call elref1(elref)
    prec=100.d0*r8prem()
    call confac(typma, ibid3, ibid, fa, nbf)
!
!     BOUCLE SUR LES FACES
    do 200 ifq = 1, nbf
!
        lajpf = .false.
        if (fa(ifq,4) .eq. 0) then
            nnof = 3
            alias = 'TR3'
        else
            nnof = 4
            alias='QU4'
        endif
        ASSERT(nnof.le.4)
!
!       RECHERCHE DES INTERSECTION ENTRE LSN ET LES LSJ SUR LA FACE
!
        somlsn = 0.d0
        do 100 i = 1, nnof
            somlsn = somlsn + abs( lsn(fa(ifq,i)) )
100     continue
        if (somlsn .eq. 0.d0) goto 200
!
        do 110 ifisc = 1, nfisc
            chgsgn = .false.
            do 120 i = 1, nnof
                na = fa(ifq,i)
                if (i .eq. 1) then
                    nb = fa(ifq,nnof)
                else
                    nb = fa(ifq,i-1)
                endif
                lsna=lsn((na-1)*nfiss+ifiss)
                lsnb=lsn((nb-1)*nfiss+ifiss)
                lsja=lsn((na-1)*nfiss+fisco(2*ifisc-1))*fisco(2*ifisc)
                lsjb=lsn((nb-1)*nfiss+fisco(2*ifisc-1))*fisco(2*ifisc)
                coorma(2*i-1)=lsja
                coorma(2*i)=lsna
!           SI LE FOND COINCIDE AVEC UN COTE DE LA FACE, ON SORT
                if (lsna .eq. 0.d0 .and. lsnb .eq. 0.d0 .and. lsja .eq. 0.d0 .and. lsjb .eq.&
                    0.d0) goto 110
!           ON ACCEPTE SI LE FRONT EST SUR UN NOEUD OU UN PT DE L'ARETE
                if (lsna .eq. 0.d0 .and. lsja .eq. 0.d0 .or. lsna .eq. 0.d0 .and. lsnb .eq.&
                    0.d0 .and. (lsja*lsjb) .lt. r8prem()) chgsgn = .true.
!           ON ACCEPTE SI UNE ARETE DE LA FACETTE EST COUPÉE
                if (abs(lsna-lsnb) .gt. r8prem() .and. (lsjb-lsnb*(lsja- lsjb)/(lsna-lsnb))&
                    .lt. prec .or. abs(lsna-lsnb) .le. r8prem() .and. (lsja*lsjb) .lt. r8prem()) &
                chgsgn = .true.
120         continue
            if (.not. chgsgn) goto 110
!
!         ON CHERCHE SUR LA MAILLE LE POINT CORRESPONDANT À LSN=LSJ=0
            mp(1)=0.d0
            mp(2)=0.d0
            call reereg('C', alias, nnof, coorma, mp,&
                        2, epsi, iret)
            if (iret .eq. 1) goto 110
!         ON NE PREND PAS EN COMPTE LES POINTS QUI SORTENT DU DOMAINE
            if (alias .eq. 'QU4') then
                if (abs(epsi(1)) .gt. 1.d0) goto 110
                if (abs(epsi(2)) .gt. 1.d0) goto 110
            else if (alias.eq.'TR3') then
                if (epsi(1) .lt. 0.d0) goto 110
                if (epsi(2) .lt. 0.d0) goto 110
                if (epsi(1)+epsi(2) .gt. 1.d0) goto 110
            endif
            mp(1)=epsi(1)
            mp(2)=epsi(2)
            call elrfvf(alias, mp, nnof, ff, nne)
            do 130 jfisc = ifisc+1, nfisc
                lsj = 0
                do 140 j = 1, nnof
                    ino = fa(ifq,j)
                    lsj = lsj + lsn( (ino-1)*nfiss+fisco(2*jfisc-1))* fisco(2*jfisc)*ff(j)
140             continue
                if (lsj .gt. 0) goto 110
130         continue
            lajpf = .true.
            do 150 i = 1, ndim
                m(i)=0
                do 160 j = 1, nnof
                    ino = fa(ifq,j)
                    m(i) = m(i) + zr(igeom-1+ndim*(ino-1)+i) * ff(j)
160             continue
150         continue
!
110     continue
!
        if (lajpf) then
!       POUR IGNORER LES POINTS CONFONDUS AVEC CEUX
!       DETECTES DANS XCFACE LORSQUE LE PT EST EXACT SUR UNE ARETE
            do 250 j = 1, ipt
                dst=padist(ndim,m,ptint(ndim*(j-1)+1))
                if (dst .le. r8prem()) lajpf = .false.
250         continue
        endif
!
        if (lajpf) then
!       ON AJOUTE A LA LISTE LE POINT M
            do 260 i = 1, ndim
                a(i) = zr(igeom-1+ndim*(fa(ifq,1)-1)+i)
                b(i) = zr(igeom-1+ndim*(fa(ifq,2)-1)+i)
                c(i) = zr(igeom-1+ndim*(fa(ifq,3)-1)+i)
260         continue
            loncar=(padist(ndim,a,b)+padist(ndim,a,c))/2.d0
            call xajpin(ndim, ptint, ptmax, ipt, ibid,&
                        m, loncar, ainter, 0, 0,&
                        0.d0, ajout)
        endif
200 continue
!
!
end subroutine
