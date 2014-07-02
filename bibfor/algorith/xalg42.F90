subroutine xalg42(ndim, elrefp, nnop, it, nnose,&
                  cnset, typma, ndime, igeom, jlsn,&
                  pmilie, ninter, ainter, ar, npts,&
                  nptm, pmmax, nmilie, mfis, lonref,&
                  pinref)
    implicit none
!
#include "asterf_types.h"
# include "jeveux.h"
# include "asterfort/jedema.h"
# include "asterfort/jemarq.h"
# include "asterfort/assert.h"
# include "asterfort/vecini.h"
# include "asterfort/xajpmi.h"
# include "asterfort/xmilar.h"
# include "asterfort/xmifis.h"
# include "asterfort/xxmmvd.h"
# include "asterfort/detefa.h"
# include "asterfort/xstudo.h"
    character(len=8) :: typma, elrefp
    integer :: ndim, ndime, nnop, it, nnose, cnset(*), igeom, jlsn
    integer :: ninter, pmmax, npts, nptm, nmilie, mfis, ar(12, 3)
    real(kind=8) :: lonref, ainter(*), pmilie(*)
    real(kind=8) :: pinref(*)
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
!                    BUT :  TROUVER LES PTS MILIEUX DANS L ELEMENT COUPE
!
!     ENTREE
!       NDIM     : DIMENSION DE L ELEMENT
!       TYPMA    : TYPE DE MAILLE
!       TABCO    : COORDONNES DES NOEUDS DE LE ELEMENT PARENT
!       PINTER   : COORDONNES DES POINTS D INTERSECTION
!       PMILIE   : COORDONNES DES POINTS MILIEUX
!       NINTER   : NOMBRE DE POINTS D INTERSECTION
!       AINTER   : INFOS ARETE ASSOCIÉE AU POINTS D'INTERSECTION
!       AR       : CONNECTIVITE DU TETRA
!       PMMAX    : NOMBRE DE POINTS MILIEUX MAXIMAL DETECTABLE
!       NPTS     : NB DE PTS D'INTERSECTION COINCIDANT AVEC UN NOEUD SOMMET
!
!     SORTIE
!       NMILIE   : NOMBRE DE POINTS MILIEUX
!       PMILIE   : COORDONNES DES POINS MILIEUX
!     ----------------------------------------------------------------
!
    real(kind=8) :: milfi(3), milara(3), milarb(3)
    real(kind=8) :: geom(81)
    real(kind=8) :: pmiref(12), ksia(ndime), ksib(ndime)
    integer :: n(3)
    integer :: i, ipm, k, j, ino
    integer :: r, a2, ip1(4), ip2(4), nbpi
    integer :: pm1a(4), pm1b(4), pm2(4)
    integer :: inm, ia, ib, mfisloc
    integer :: zxain
    aster_logical :: ispm2, ajout
!
! --------------------------------------------------------------------
!
    call jemarq()
!
    zxain = xxmmvd('ZXAIN')
!     COMPTEUR DES POINTS INTERSECTION NON CONFONDUS AVEC ND SOMMET
!     COMPTEUR DE POINTS MILIEUX
    ipm=0
    inm=0
    mfisloc=0
!
    call vecini(51, 0.d0, pmilie)
!
    do ino = 1, nnop
        do i = 1, ndim
            geom(ndim*(ino-1)+i)=zr(igeom-1+ndim*(ino-1)+i)
        enddo
    enddo
!
    do 204 i = 1, 4
        ip1(i)=0
        ip2(i)=0
        pm1a(i)=0
        pm1b(i)=0
        pm2(i)=0
204 continue
    call xstudo(ndime, ninter, npts, nptm, ainter,&
                nbpi, ip1, ip2, pm1a, pm1b,&
                pm2)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    RECHERCHE DU PREMIER TYPE DE POINT MILIEU    !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    do 300 r = 1, ninter
        a2=nint(ainter(zxain*(r-1)+1))
! POUR EMPECHER L ALGO DE CALCULER TROP DE POINTS MILIEUX 
! COMME LE NOEUD MILIEU EST EN 4EME POSITION QUAND NINTER=4,NPTS=2
        if (r .eq. 4) goto 300
        if (a2 .eq. 0) goto 300
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  REMARQUE IMPORTANTE ::                                                                         !
!  ON NE PEUT PAS DEFINIR UNE REFERENCE POUR L ORDONNACEMENT DANS CETTE CONFIGURATION DE DECOUPE  !
!  CAR LES DEUX DOMAINES DE DECOUPE SONT SYMETRIQUES (DEUX SOUS-TETRAS SYMETRIQUES)               !
!  L ORDONANCEMENT DES NOEUDS MILIEUX SUR L ARETE EST ALORS IMPLICITE                             !
!  IL DEPEND EN FAIT DE L ORDONANCEMMENT DES NOEUDS DANS LE TABLEAU AR                            !
!  ON A :                                                                                         !
!   IP1=CONNEC(AR(I,1))                                                                           !
!   IP2=CONNEC(AR(I,2))                                                                           !
!   PM1 EST MILIEU DE AR(A2,1) ET IP3                                                             !
!   PM2 EST MILIEU DE AR(A2,2) ET IP3                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ia=cnset(nnose*(it-1)+ar(a2,1))
        ib=cnset(nnose*(it-1)+ar(a2,2))
        call vecini(ndim, 0.d0, milara)
        call vecini(ndim, 0.d0, milarb)
!    ORDONANCEMENT DES NOEUDS MILIEUX SUR L ARETE : RECHERCHE DU NOEUD A SUR L ARETE A2
        call xmilar(ndim, ndime, elrefp, geom, pinref,&
                    ia, ib, r, ksia, ksib,&
                    milara, milarb)
!         STOCKAGE PMILIE
        call xajpmi(pmilie, pmmax, ipm, inm, milara,&
                    lonref, ajout)
        if (ajout) then
            do j = 1, ndime
                pmiref(ndime*(ipm-1)+j)=ksia(j)
            enddo
        endif
        call xajpmi(pmilie, pmmax, ipm, inm, milarb,&
                    lonref, ajout)
        if (ajout) then
            do j = 1, ndime
                pmiref(ndime*(ipm-1)+j)=ksib(j)
            enddo
        endif
300 continue
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    RECHERCHE DU DEUXIEME TYPE DE POINT MILIEU    !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    do 400 k = 1, nbpi
!      POINT MILIEU ENTRE IP1(R) ET IP2(R)
!
!      on ne calcule pas le premier type de point milieu si la
!      fissure coincide avec une arete
        ispm2=(nint(ainter(zxain*(ip1(k)-1)+1)) .ne. 0).or. &
           (nint(ainter(zxain*(ip2(k)-1)+1)) .ne. 0)
!
!
        if (ispm2) then
!        DETECTER LA COTE PORTANT LES DEUX POINTS D'INTERSECTIONS
            call detefa(nnose, ip1(k), ip2(k), it, typma,&
                        ainter, cnset, n)
!
!        CALCUL DU POINT MILIEU DE 101-102
!
            call xmifis(ndim, ndime, elrefp, geom, zr(jlsn),&
                        n, ip1(k), ip2(k), pinref, ksia,&
                        milfi)
!
!        on incremente le nombre de points milieux sur la fissure
            mfisloc=mfisloc+1
!        STOCKAGE PMILIE
            call xajpmi(pmilie, pmmax, ipm, inm, milfi,&
                        lonref, ajout)
            if (ajout) then
                do j = 1, ndime
                    pmiref(ndime*(ipm-1)+j)=ksia(j)
                enddo
            endif
        endif
400 continue
!
    ASSERT(ipm.eq.4.and.mfisloc.eq.2)
!
    nmilie = ipm
    mfis=mfis+mfisloc
!
    call jedema()
end subroutine
