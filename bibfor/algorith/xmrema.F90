subroutine xmrema(jcesd, jcesv, jcesl, noma, ndim,&
                  ifise, defico, izone, alias, mmait,&
                  amait, nmait, statue, geom, nummin,&
                  nummae, ifamin, ifacee, jeumin, t1min,&
                  t2min, ximin, yimin, projin, stamin,&
                  ifism)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
!
#include "asterc/r8gaem.h"
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisr.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/conare.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/mminfr.h"
#include "asterfort/mmjeux.h"
#include "asterfort/mmproj.h"
#include "asterfort/normev.h"
#include "asterfort/panbno.h"
    character(len=8) :: alias, noma
    integer :: ndim, mmait, nmait, amait, statue, stamin
    character(len=24) :: defico
    integer :: jcesd(10), jcesv(10), jcesl(10), ifise
    integer :: izone
    real(kind=8) :: geom(3)
    integer :: nummin, ifamin, nummae, ifacee, ifism
    real(kind=8) :: jeumin
    real(kind=8) :: t1min(3), t2min(3)
    real(kind=8) :: ximin, yimin
    logical :: projin
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (CONTACT - GRANDS GLISSEMENTS)
!
! RECHERCHER LA MAILLE MAITRE LA PLUS PROCHE CONNAISSANT LE POINT
! D'INTERSECTION MAITRE LE PLUS PROCHE DU POINT DE CONTACT ET FAIRE
! LA PROJECTION
!
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
!
! ----------------------------------------------------------------------
!
!
!  JCES*(1)  : POINTEURS DE LA SD SIMPLE NB DE FACETTES ET DE PT D'INTER
!  JCES*(4)  : POINTEURS DE LA SD SIMPLE DE CONNECTIVITÉ DES FACETTES
!  JCES*(6)  : POINTEURS DE LA SD SIMPLE DES COOR DES PT D'INTER MAITRE
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  ALIAS  : TYPE DE MAILLE DE CONTACT
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  IFISE  : NUMEROS DE FISSURE LOCALE DE LA MAILLE ESCLAVE
! IN  IZONE  : ZONE DE CONTACT DU POINT D'INTEGRATION ESCLAVE
! IN  STATUE : STATUM DE LA MAILLE ESCLAVE
! IN  PMAIT  : NUMERO LOCAL DU POINT D'INTERSECTION LE PLUS PROCHE
! IN  AMAIT  : NUMERO LOCAL DE L'ARETE INTERSECTÉ
! IN  NMAIT  : NUMERO LOCAL DU NOEUD INTERSECTÉ
! IN  MMAIT  : NUMERO DE LA MAILLE MAITRE CONTENANT LE PMAIT
! IN  GEOM   : COORDONNEES DU POINT DE CONTACT
! IN  NUMMAE : NUMERO ABSOLU DANS LE MAILLAGE DE LA MAILLE ESCLAVE
! IN  IFACE  : NUMERO LOCAL DE LA FACETTE ESCLAVE
! OUT NUMMIN : NUMERO ABSOLU DANS LE MAILLAGE DE LA MAILLE MAITRE
!              LA PLUS PROCHE
! OUT IFAMIN : NUMERO LOCAL DE LA FACETTE MAITRE LA PLUS PROCHE
! OUT JEUMIN : JEU MINIMUM
! OUT T1MIN  : PREMIER VECTEUR TANGENT
! OUT T2MIN  : DEUXIEME VECTEUR TANGENT
! OUT XIMIN  : COORDONNEE X DE LE PROJECTION MINIMALE DU POINT DE
!              CONTACT SUR LA MAILLE MAITRE
! OUT YIMIN  : COORDONNEE Y DE LE PROJECTION MINIMALE DU POINT DE
!              CONTACT SUR LA MAILLE MAITRE
! OUT PROJIN : VAUT .TRUE. SI LA PROJECTION DU POINT DE CONTACT N'EST
!              PAS LE RESULTAT DU RABATTEMENT
!              .FALSE. S'IL Y A EU RABATTEMENT PARCE QU'ELLE SERAIT
!              TOMBEEE HORS DE LA MAILLE MAITRE (A LA TOLERANCE PRES)
! OUT STAMIN : STATUT DE LA MAILLE MAITRE RETENUE
!
!
!
!
    integer :: itemax
    integer :: zmesx
    character(len=24) :: maescx
    integer :: jmaesx
    integer :: statum
    integer :: jconx1, jconx2
    integer :: nummai, nunoin, nunog, nugla, nuglb, nbnott(3)
    integer :: n1, n2, nbnos, ntmae, nfacem
    integer :: ino, ifacem, ima
    integer :: i, j, k, ia, numpi(3), niverr
    integer :: ar(12, 3), nbar, na, nb, nunoa, nunob
    real(kind=8) :: jeu, tau1(3), tau2(3)
    real(kind=8) :: toleou, epsmax, nrese
    real(kind=8) :: coorma(27), xi, yi
    real(kind=8) :: r3bid(3)
    character(len=8) :: typma
    logical :: dirapp, noapar, lappar
    integer :: iproj, iprojm
    integer :: iad, jma, itypma, nptm, ifiss
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    jeumin = r8gaem()
    projin = .false.
    nummin = 0
    lappar = .false.
    do 10 i = 1, 27
        coorma(i)=0.d0
10  end do
    dirapp = .false.
    ntmae = cfdisi(defico,'NTMAE')
!
! --- RECUPERATION DE QUELQUES DONNEES
!
    maescx = defico(1:16)//'.MAESCX'
    call jeveuo(maescx, 'L', jmaesx)
    zmesx = cfmmvd('ZMESX')
!
! --- INFOS GENERIQUES POUR L'ALGORITHME D'APPARIEMENT
!
    toleou = mminfr(defico,'TOLE_PROJ_EXT' ,izone)
    epsmax = cfdisr(defico,'PROJ_NEWT_RESI')
    itemax = cfdisi(defico,'PROJ_NEWT_ITER')
!
    if (statue .eq. 2 .or. statue .lt. 0) then
!
! --- ELEMENT EXCLUSIVEMENT CRACK-TIP, ON PROJETTE SUR LUI-MEME
!
        do 240 i = 1, ndim
            call cesexi('S', jcesd(4), jcesl(4), nummae, 1,&
                        ifise, ( ifacee-1)*ndim+i, iad)
            call assert(iad.gt.0)
            numpi(i) = zi(jcesv(4)-1+iad)
240      continue
        do 250 i = 1, ndim
            do 260 j = 1, ndim
                call cesexi('S', jcesd(6), jcesl(6), nummae, 1,&
                            ifise, ndim*(numpi(i)-1)+j, iad)
                call assert(iad.gt.0)
                coorma(3*(i-1)+j)=zr(jcesv(6)-1+iad)
260          continue
250      continue
        call mmproj(alias, ndim, ndim, coorma, geom,&
                    itemax, epsmax, toleou, dirapp, r3bid,&
                    ximin, yimin, t1min, t2min, iprojm,&
                    niverr)
        if (niverr .eq. 1) then
            call assert(.false.)
        endif
        nummin = nummae
        ifamin = ifacee
        stamin = statue
        ifism = ifise
        if (statue .eq. 2) projin = .true.
        goto 999
    endif
!
! --- ON RECUPERE LA CONNECTIVITE DU MAILLAGE
!
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
! ----- NB: ON SE BASE SUR LE TYPE DE LA MAILLE MAITRE.
!
    call jeveuo(noma//'.TYPMAIL', 'L', jma)
    itypma = zi(jma-1+mmait)
    call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
!
! ----- SI LE POINT DE CONTACT EST SUR UNE ARETE
!
    if (amait .gt. 0) then
        call conare(typma, ar, nbar)
        na = ar(amait,1)
        nb = ar(amait,2)
        nunoa = zi(jconx1-1+zi(jconx2+mmait-1)+na-1)
        nunob = zi(jconx1-1+zi(jconx2+mmait-1)+nb-1)
!
! ----- SI LE POINT DE CONTACT EST SUR UN NOEUD
!
    else if (nmait.gt.0) then
        nunog = zi(jconx1-1+zi(jconx2+mmait-1)+nmait-1)
        call panbno(itypma, nbnott)
        nbnos = nbnott(1)
        if (typma .eq. 'QUAD8') nbnos=8
        if (typma .eq. 'TRIA6') nbnos=6
    else
        call assert(.false.)
    endif
!
200  continue
!
! --- BOUCLE SUR LES MAILLES FISSURÉES
!
    do 100 ima = 1, ntmae
!
! --- SI CE N'EST PAS LA BONNE ZONE, ON SORT
!
        if (zi(jmaesx+zmesx*(ima-1)+2-1) .ne. izone) goto 100
!
        nummai = zi(jmaesx+zmesx*(ima-1)+1-1)
        statum = zi(jmaesx+zmesx*(ima-1)+4-1)
        ifiss = zi(jmaesx+zmesx*(ima-1)+5-1)
!
        noapar = .true.
        if (lappar) noapar = .false.
!
        if (amait .gt. 0) then
!
! ----- SI LE POINT DE CONTACT EST SUR UNE ARÊTE
! ----- ON BOUCLE SUR LES ARETES DE LA MAILLE COURANTE
! ----- ON REGARDE SI L'ARETE APPARTIENT A CETTE MAILLE
!
            do 110 ia = 1, nbar
                n1 = ar(ia,1)
                n2 = ar(ia,2)
                nugla = zi(jconx1-1+zi(jconx2+nummai-1)+n1-1)
                nuglb = zi(jconx1-1+zi(jconx2+nummai-1)+n2-1)
!
                if (((nugla.eq.nunoa).and.(nuglb.eq.nunob)) .or.&
                    ((nugla.eq.nunob).and.(nuglb.eq.nunoa))) then
                    noapar=.false.
                endif
110          continue
        else
!
! ----- SI LE POINT DE CONTACT EST UN NOEUD
! ----- ON BOUCLE SUR LES NOEUDS DE LA MAILLE COURANTE
! ----- ON REGARDE SI LE NOEUD APPARTIENT A CETTE MAILLE
!
            do 120 ino = 1, nbnos
                nunoin=zi(jconx1-1+zi(jconx2+nummai-1)+ino-1)
                if (nunoin .eq. nunog) then
                    noapar=.false.
                endif
120          continue
        endif
!
        if (noapar) goto 100
!
! ----- RECUPERATION DU NOMBRE DE FACETTES DE CONTACT DE LA MAILLE
!
        call cesexi('S', jcesd(1), jcesl(1), nummai, 1,&
                    ifiss, 2, iad)
        call assert(iad.gt.0)
        nfacem = zi(jcesv(1)-1+iad)
        call cesexi('S', jcesd(1), jcesl(1), nummai, 1,&
                    ifiss, 3, iad)
        call assert(iad.gt.0)
        nptm = zi(jcesv(1)-1+iad)
!
! ----- BOUCLE SUR LES FACETTES DE CONTACT DE LA MAILLE COURANTE
!
        do 130 ifacem = 1, nfacem
!
! ----- RECUPERATION DES NUMEROS LOCAUX DES POINTS D'INTERSECTIONS
! ----- DE LA FACETTE DANS LA MAILLE
!
            do 140 i = 1, nptm
                call cesexi('S', jcesd(4), jcesl(4), nummai, 1,&
                            ifiss, (ifacem-1)*nptm+i, iad)
                call assert(iad.gt.0)
                numpi(i) = zi(jcesv(4)-1+iad)
140          continue
!
! ----- RECUPERATION DES COORDONNES REELLES DES POINTS D'INTERSECTION
! ----- DE LA FACETTE MAITRE
!
            do 150 i = 1, nptm
                do 160 j = 1, ndim
                    call cesexi('S', jcesd(6), jcesl(6), nummai, 1,&
                                ifiss, ndim*(numpi(i)-1)+j, iad)
                    call assert(iad.gt.0)
                    coorma(3*(i-1)+j)=zr(jcesv(6)-1+iad)
160              continue
150          continue
!
! --- PROJECTION SUR LA FACETTE MAITRE
!
            call mmproj(alias, nptm, ndim, coorma, geom,&
                        itemax, epsmax, toleou, dirapp, r3bid,&
                        xi, yi, tau1, tau2, iproj,&
                        niverr)
!
! --- ECHEC DE NEWTON
!
            if (niverr .eq. 1) then
                call assert(.false.)
            endif
!
! --- CHOIX DE LA MAILLE
!
            if (statum .eq. 2) then
! --- ON EST EN FACE D'UNE FACETTE EXCLUSIVEMENT CT,
! --- ON N'APPARIE PAS MAIS ON ACTIVE LE CONTACT
                if (iproj .le. 1) projin = .true.
            else
!
! --- CALCUL DU JEU
!
                call mmjeux(alias, nptm, ndim, coorma, xi,&
                            yi, geom, jeu, r3bid)
                if (jeu .lt. jeumin) then
                    nummin = nummai
                    ifamin = ifacem
                    jeumin = jeu
                    iprojm = iproj
                    stamin = statum
                    ifism = ifiss
                    do 40 k = 1, 3
                        t1min(k) = tau1(k)
                        t2min(k) = tau2(k)
40                  continue
                    ximin = xi
                    yimin = yi
                endif
            endif
!
130      continue
100  end do
!
    if (nummin .eq. 0 .and. (.not.lappar)) then
!       DEUXIÈME CHANCE
        lappar = .true.
        goto 200
    endif
    if (iprojm .le. 1) projin = .true.
    if (toleou .eq. -1.d0) projin = .true.
!
999  continue
!
! --- NORMALISATION DES VECTEURS TANGENTS
!
    call normev(t1min, nrese)
    call normev(t2min, nrese)
!
    call jedema()
end subroutine
