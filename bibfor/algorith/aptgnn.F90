subroutine aptgnn(sdappa, noma, defico, ndimg, jdecno,&
                  nbno, itype, vector)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'jeveux.h'
!
    include 'asterc/r8prem.h'
    include 'asterfort/apatta.h'
    include 'asterfort/apninv.h'
    include 'asterfort/apnndm.h'
    include 'asterfort/apnumm.h'
    include 'asterfort/apnumn.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/mmmron.h'
    include 'asterfort/mmnorm.h'
    include 'asterfort/mmtann.h'
    include 'asterfort/normev.h'
    include 'asterfort/provec.h'
    include 'asterfort/u2mesk.h'
    include 'blas/dcopy.h'
    character(len=19) :: sdappa
    character(len=8) :: noma
    character(len=24) :: defico
    integer :: ndimg, jdecno, nbno, itype
    real(kind=8) :: vector(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT - TANGENTES EN CHAQUE NOEUD
!
! CALCUL SUR UNE ZONE
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  NOMA   : SD MAILLAGE
! IN  DEFICO : SD DEFINITION DU CONTACT
! IN  NMDIMG : DIMENSION DE L'ESPACE
! IN  JDECNO : DECALAGE POUR NUMERO DE NOEUD
! IN  NBNO   : NOMBRE DE NOEUDS DE LA ZONE
! IN  ITYPE  : NORMALE 'AUTO'(0)/'FIXE'(1)/'VECT_Y'(2)
! IN  VECTOR : POUR ITYPE = 1 OU 2
!
!
!
!
    character(len=8) :: nomnoe, nommai, valk(2)
    integer :: posmai, nummai, posno, numno
    integer :: nmanom, nnosdm
    integer :: jdeciv, jdec
    integer :: ino, ima, inocou, inomai
    integer :: niverr
    real(kind=8) :: tau1(3), tau2(3), normal(3), normn
    real(kind=8) :: taund1(3), taund2(3)
    real(kind=8) :: vnorm(3), noor
    character(len=24) :: aptgel, aptgno
    integer :: jtgeln, jptgno
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD
!
    aptgel = sdappa(1:19)//'.TGEL'
    aptgno = sdappa(1:19)//'.TGNO'
    call jeveuo(aptgno, 'E', jptgno)
!
! --- BOUCLE SUR LES NOEUDS
!
    do 20 ino = 1, nbno
!
! ----- INITIALISATIONS
!
        normal(1) = 0.d0
        normal(2) = 0.d0
        normal(3) = 0.d0
        taund1(1) = 0.d0
        taund1(2) = 0.d0
        taund1(3) = 0.d0
        taund2(1) = 0.d0
        taund2(2) = 0.d0
        taund2(3) = 0.d0
!
! ----- NOEUD COURANT
!
        posno = ino+jdecno
!
! ----- NUMERO ABSOLU ET NOM DU NOEUD
!
        call apnumn(sdappa, defico, posno, numno)
        call jenuno(jexnum(noma//'.NOMNOE', numno ), nomnoe)
!
! ----- NOMBRE DE MAILLES ATTACHEES AU NOEUD
!
        call apninv(sdappa, defico, posno, 'NMANOM', nmanom)
!
! ----- DECALAGE POUR CONNECTIVITE INVERSE
!
        call apninv(sdappa, defico, posno, 'JDECIV', jdeciv)
!
! ----- BOUCLE SUR LES MAILLES ATTACHEES
!
        do 10 ima = 1, nmanom
!
! ------- POSITION DE LA MAILLE ATTACHEE
!
            call apatta(sdappa, defico, jdeciv, ima, posmai)
!
! ------- NUMERO ABSOLU ET NOM DE LA MAILLE ATTACHEE
!
            call apnumm(sdappa, defico, posmai, nummai)
            call jenuno(jexnum(noma//'.NOMMAI', nummai), nommai)
            valk(1) = nommai
            valk(2) = nomnoe
!
! ------- ACCES CONNECTIVITE DE LA MAILLE ATTACHEE
!
            call jeveuo(jexnum(noma//'.CONNEX', nummai), 'L', jdec)
!
! ------- NOMBRE DE NOEUDS DE LA MAILLE ATTACHEE
!
            call apnndm(sdappa, defico, posmai, nnosdm)
!
! ------- ACCES TANGENTES MAILLE COURANTE
!
            call jeveuo(jexnum(aptgel, posmai), 'L', jtgeln)
!
! ------- TRANSFERT NUMERO ABSOLU DU NOEUD -> NUMERO DANS LA CONNEC DE
! ------- LA MAILLE
!
            inocou = 0
            do 30 inomai = 1, nnosdm
                if (zi(jdec+inomai-1) .eq. numno) then
                    inocou = inomai
                endif
30          continue
            call assert(inocou.ne.0)
!
! ------- RECUPERATIONS DES TANGENTES EN CE NOEUD
!
            tau1(1) = zr(jtgeln+6*(inocou-1)+1-1)
            tau1(2) = zr(jtgeln+6*(inocou-1)+2-1)
            tau1(3) = zr(jtgeln+6*(inocou-1)+3-1)
            tau2(1) = zr(jtgeln+6*(inocou-1)+4-1)
            tau2(2) = zr(jtgeln+6*(inocou-1)+5-1)
            tau2(3) = zr(jtgeln+6*(inocou-1)+6-1)
!
! ------- CALCUL DE LA NORMALE _INTERIEURE_
!
            call mmnorm(ndimg, tau1, tau2, vnorm, noor)
            if (noor .le. r8prem()) then
                call u2mesk('F', 'APPARIEMENT_15', 2, valk)
            endif
!
! ------- NORMALE RESULTANTE
!
            normal(1) = normal(1) + vnorm(1)
            normal(2) = normal(2) + vnorm(2)
            normal(3) = normal(3) + vnorm(3)
10      continue
!
! ----- MOYENNATION DE LA NORMALE SUR TOUTES LES MAILLES LIEES AU NOEUD
!
        normal(1) = normal(1) / nmanom
        normal(2) = normal(2) / nmanom
        normal(3) = normal(3) / nmanom
!
! ----- NORMALISATION NORMALE SUR TOUTES LES MAILLES LIEES AU NOEUD
!
        call normev(normal, normn)
        if (normn .le. r8prem()) then
            call u2mesk('F', 'APPARIEMENT_16', 1, nomnoe)
        endif
!
! ----- RE-CONSTRUCTION DES VECTEURS TANGENTS APRES LISSAGE
!
        call mmmron(ndimg, normal, taund1, taund2)
!
! ----- CAS PARTICULIER VECT_Y : ON REDEFINIT TAUND2
!
        if (itype .eq. 2) then
            call dcopy(3, vector, 1, taund2, 1)
            call provec(normal, taund2, taund1)
        endif
!
! ----- NORMALISATION DES TANGENTES
!
        call mmtann(ndimg, taund1, taund2, niverr)
        if (niverr .eq. 1) then
            call u2mesk('F', 'APPARIEMENT_17', 1, nomnoe)
        endif
!
! ----- STOCKAGE DES VECTEURS TANGENTS EXTERIEURS SUR LE NOEUD
!
        zr(jptgno+6*(posno-1)+1-1) = taund1(1)
        zr(jptgno+6*(posno-1)+2-1) = taund1(2)
        zr(jptgno+6*(posno-1)+3-1) = taund1(3)
        zr(jptgno+6*(posno-1)+4-1) = taund2(1)
        zr(jptgno+6*(posno-1)+5-1) = taund2(2)
        zr(jptgno+6*(posno-1)+6-1) = taund2(3)
20  end do
!
    call jedema()
end subroutine
