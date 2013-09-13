subroutine te0378(option, nomte)
! ----------------------------------------------------------------------
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
! person_in_charge: josselin.delmas at edf.fr
!
!     BUT:
!       CALCUL DE L'INDICATEUR D'ERREUR EN MECANIQUE 2D AVEC LA
!       METHODE EN QUANTITE D'INTERET BASEE SUR LES RESIDUS EXPLICITES.
!       OPTION : 'QIRE_ELEM'
!
! REMARQUE : LES PROGRAMMES SUIVANTS DOIVENT RESTER TRES SIMILAIRES
!            TE0368, TE0375, TE0377, TE0378, TE0382, TE0497
!
! ----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterfort/calnor.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elref1.h"
#include "asterfort/elref4.h"
#include "asterfort/elref7.h"
#include "asterfort/ermeb2.h"
#include "asterfort/ermes2.h"
#include "asterfort/ermev2.h"
#include "asterfort/fointe.h"
#include "asterfort/infniv.h"
#include "asterfort/intenc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jevech.h"
#include "asterfort/jexnum.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/resrot.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/uthk.h"
#include "asterfort/utjac.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option, nomte
!
!
!
! DECLARATION VARIABLES LOCALES
!
    integer :: ifm, niv
    integer :: iadzi, iazk24
    integer :: ibid, iaux, iret, itab(7)
    integer :: igeom, jtime
    integer :: ierr, ivois
    integer :: imate
    integer :: iadp
    integer :: iadd
    integer :: ifovrp, ifovfp
    integer :: ifovrd, ifovfd
    integer :: ipesp, irotp
    integer :: ipesd, irotd
    integer :: irefp1, irefp2
    integer :: irefd1, irefd2
    integer :: ndim
    integer :: nno, nnos, npg, ipoids, ivf, idfde, jgano
    integer :: nnof
    integer :: nbcmp
    integer :: ipg, in
    integer :: nbf
    integer :: tymvol, ndegre, ifa, tyv, noe(9, 6, 4)
!
    real(kind=8) :: r8bid, r8bid3(2), r8bid4(2)
    real(kind=8) :: dfdx(9), dfdy(9), hk, poids
    real(kind=8) :: fppx, fppy
    real(kind=8) :: fpdx, fpdy
    real(kind=8) :: frpx(9), frpy(9)
    real(kind=8) :: frdx(9), frdy(9)
    real(kind=8) :: fovop(2)
    real(kind=8) :: fovod(2)
    real(kind=8) :: dspx, dspy
    real(kind=8) :: dsdx, dsdy
    real(kind=8) :: s, unsurs
    real(kind=8) :: nx(9), ny(9), nz(9), tx(3), ty(3), jaco(9)
    real(kind=8) :: chpx(3), chpy(3), chdx(3), chdy(3)
    real(kind=8) :: sgp11(3), sgp22(3), sgp12(3)
    real(kind=8) :: sgd11(3), sgd22(3), sgd12(3)
    real(kind=8) :: sigp11(3), sigp22(3), sigp12(3)
    real(kind=8) :: sigd11(3), sigd22(3), sigd12(3)
    real(kind=8) :: chplx(3), chply(3), chmox(3), chmoy(3)
    real(kind=8) :: sopl11(3), sopl22(3), sopl12(3)
    real(kind=8) :: somo11(3), somo22(3), somo12(3)
    real(kind=8) :: sipl11(3), sipl22(3), sipl12(3)
    real(kind=8) :: simo11(3), simo22(3), simo12(3)
    real(kind=8) :: nuplus, numoin
    real(kind=8) :: rho, valres(1)
    real(kind=8) :: errest, coeff, orien
    real(kind=8) :: hf, intpl, intmo, inst
    real(kind=8) :: terpl1, termo1, terpl2, termo2, terpl3, termo3
!
    integer :: icodre(2), kpg, spt
    character(len=8) :: typmav, elrefe, fami, poum
    character(len=8) :: elreff, elrefb
    character(len=8) :: nompar(1)
    character(len=16) :: phenom
    character(len=24) :: valk(2)
!
    logical :: yaprp, yarop
    logical :: yaprd, yarod
!
! ----------------------------------------------------------------------
    1000 format(a,' :',(6(1x,1pe17.10)))
! ----------------------------------------------------------------------
! 1 -------------- GESTION DES DONNEES ---------------------------------
! ----------------------------------------------------------------------
    call jemarq()
!
    call infniv(ifm, niv)
!
! 1.1. --- LES INCONTOURNABLES
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PVOISIN', 'L', ivois)
!
    call jevech('PTEMPSR', 'L', jtime)
    inst=zr(jtime-1+1)
!
    call jevech('PERREUR', 'E', ierr)
!
! 1.2. --- LES CARACTERISTIQUES DE LA MAILLE EN COURS
!
    call tecael(iadzi, iazk24)
    valk(1)=zk24(iazk24-1+3)
    valk(2)=option
!
    call elref1(elrefe)
!
    if (niv .ge. 2) then
        write(ifm,*) ' '
        write(ifm,*) '================================================='
        write(ifm,*) ' '
        write(ifm,*) 'MAILLE NUMERO', zi(iadzi),', DE TYPE ', elrefe
    endif
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
! 1.3. --- CHAMP DE CONTRAINTES
!
    call tecach('OOO', 'PCONTNOP', 'L', 3, itab,&
                iret)
    iadp=itab(1)
    nbcmp=itab(2)/nno
    call tecach('OOO', 'PCONTNOD', 'L', 3, itab,&
                iret)
    iadd=itab(1)
!
! 1.4. --- CARTES DE PESANTEUR ET ROTATION
!
    call tecach('ONN', 'PPESANRP', 'L', 1, itab,&
                iret)
    if (itab(1) .ne. 0) then
        call jevech('PPESANRP', 'L', ipesp)
        yaprp = .true.
    else
        yaprp = .false.
    endif
    call tecach('ONN', 'PROTATRP', 'L', 1, itab,&
                iret)
    if (itab(1) .ne. 0) then
        call jevech('PROTATRP', 'L', irotp)
        yarop = .true.
    else
        yarop = .false.
    endif
    call tecach('ONN', 'PPESANRD', 'L', 1, itab,&
                iret)
    if (itab(1) .ne. 0) then
        call jevech('PPESANRD', 'L', ipesd)
        yaprd = .true.
    else
        yaprd = .false.
    endif
    call tecach('ONN', 'PROTATRD', 'L', 1, itab,&
                iret)
    if (itab(1) .ne. 0) then
        call jevech('PROTATRD', 'L', irotd)
        yarod = .true.
    else
        yarod = .false.
    endif
!
! 1.5. --- FORCES VOLUMIQUES EVENTUELLES
!          VALEURS REELLES ?
    call tecach('ONN', 'PFRVOLUP', 'L', 1, ifovrp,&
                iret)
!          OU FONCTIONS ?
    if (ifovrp .eq. 0) then
        call tecach('ONN', 'PFFVOLUP', 'L', 1, ifovfp,&
                    iret)
    else
        ifovfp = 0
    endif
!          VALEURS REELLES ?
    call tecach('ONN', 'PFRVOLUD', 'L', 1, ifovrd,&
                iret)
!          OU FONCTIONS ?
    if (ifovrd .eq. 0) then
        call tecach('ONN', 'PFFVOLUD', 'L', 1, ifovfd,&
                    iret)
    else
        ifovfd = 0
    endif
!
! 1.6. --- FORCES ET PRESSIONS AUX BORDS
!
    call jevech('PFORCEP', 'L', irefp1)
    call jevech('PFORCED', 'L', irefd1)
!
    call jevech('PPRESSP', 'L', irefp2)
    call jevech('PPRESSD', 'L', irefd2)
!
! 1.7. --- MATERIAU SI BESOIN
!
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    if (yaprp .or. yarop .or. yaprd .or. yarod) then
!
        call jevech('PMATERC', 'L', imate)
        call rccoma(zi(imate), 'ELAS', 1, phenom, icodre(1))
        nompar(1)='RHO'
        call rcvalb(fami, kpg, spt, poum, zi(imate),&
                    ' ', phenom, 1, ' ', r8bid,&
                    1, nompar, valres, icodre, 1)
        rho = valres(1)
!GN        WRITE(IFM,1000) 'RHO', RHO
!
    endif
!
! 1.8. --- CALCUL DU COEFFICIENT S
!
    call jevech('PCONSTR', 'L', ibid)
    s = zr(ibid-1+1)
    unsurs = 1.d0/s
!
! ----------------------------------------------------------------------
! 2 -------------- CALCUL DU PREMIER TERME DE L'ERREUR -----------------
! ----------------------------------------------------------------------
!
! 2.1. --- CALCUL DU DIAMETRE HK DE LA MAILLE ----
!
    call uthk(nomte, zr(igeom), hk, ndim, itab,&
              ibid, ibid, ibid, niv, ifm)
!
! 2.2. --- CALCUL DE LA FORCE DE PESANTEUR  ---
!
! ------ CALCUL DE LA FORCE DE PESANTEUR PB. PRIMAL --------------------
    if (yaprp) then
        fppx=rho*zr(ipesp)*zr(ipesp+1)
        fppy=rho*zr(ipesp)*zr(ipesp+2)
    else
        fppx=0.d0
        fppy=0.d0
    endif
!GN      WRITE(IFM,1000) 'P PRIMAL',FPPX,FPPY
! ------ CALCUL DE LA FORCE DE PESANTEUR PB. DUAL ----------------------
    if (yaprd) then
        fpdx=rho*zr(ipesd)*zr(ipesd+1)
        fpdy=rho*zr(ipesd)*zr(ipesd+2)
    else
        fpdx=0.d0
        fpdy=0.d0
    endif
!GN      WRITE(IFM,1000) 'P DUAL  ',FPDX,FPDY
!
! 2.3. --- CALCUL DE LA FORCE DE ROTATION ---
! ------ CALCUL DE LA FORCE DE ROTATION AUX POINTS DE GAUSS PB. PRIMAL -
!
    if (yarop) then
        call resrot(zr(irotp), zr(igeom), zr(ivf), rho, nno,&
                    npg, frpx, frpy)
    else
        do 231 , ipg = 1 , npg
        frpx(ipg) = 0.d0
        frpy(ipg) = 0.d0
231      continue
    endif
!GN      WRITE(IFM,1000) 'R PRIMAL X',(FRPX(IPG),IPG = 1 , NPG)
!GN      WRITE(IFM,1000) 'R PRIMAL Y',(FRPY(IPG),IPG = 1 , NPG)
! ------ CALCUL DE LA FORCE DE ROTATION AUX POINTS DE GAUSS PB. DUAL ---
    if (yarod) then
        call resrot(zr(irotd), zr(igeom), zr(ivf), rho, nno,&
                    npg, frdx, frdy)
    else
        do 232 , ipg = 1 , npg
        frdx(ipg) = 0.d0
        frdy(ipg) = 0.d0
232      continue
    endif
!GN      WRITE(IFM,1000) 'R DUAL X  ',(FRDX(IPG),IPG = 1 , NPG)
!GN      WRITE(IFM,1000) 'R DUAL Y  ',(FRDY(IPG),IPG = 1 , NPG)
!
! 2.4. --- CALCUL DE LA FORCE VOLUMIQUE EVENTUELLE ---
!
    if (ifovrp .ne. 0) then
        fovop(1) = zr(ifovrp )
        fovop(2) = zr(ifovrp+1)
!
    else if (ifovfp.ne.0) then
        nompar(1) = 'INST'
        r8bid3(1) = inst
!       SI UNE COMPOSANTE N'A PAS ETE DECRITE, ASTER AURA MIS PAR
!       DEFAUT LA FONCTION NULLE &FOZERO. ON LE REPERE POUR
!       IMPOSER LA VALEUR 0 SANS FAIRE DE CALCULS INUTILES
        do 241 , ibid = 1 , ndim
        if (zk8(ifovfp+ibid-1)(1:7) .eq. '&FOZERO') then
            fovop(ibid) = 0.d0
        else
            call fointe('FM', zk8(ifovfp+ibid-1), 1, nompar, r8bid3,&
                        fovop(ibid), iret)
        endif
241      continue
!GN        WRITE(IFM,*) 'F PRIMAL X : ',ZK8(IFOVFP)
!GN        WRITE(IFM,*) 'F PRIMAL Y : ',ZK8(IFOVFP+1)
    endif
!GN      WRITE(IFM,2000) 'IFOVRP', IFOVRP
!GN      WRITE(IFM,2000) 'IFOVFP', IFOVRP
!
    if (ifovrd .ne. 0) then
        fovod(1) = zr(ifovrd )
        fovod(2) = zr(ifovrd+1)
!
    else if (ifovfd.ne.0) then
        nompar(1) = 'INST'
        r8bid3(1) = inst
!       SI UNE COMPOSANTE N'A PAS ETE DECRITE, ASTER AURA MIS PAR
!       DEFAUT LA FONCTION NULLE &FOZERO. ON LE REPERE POUR
!       IMPOSER LA VALEUR 0 SANS FAIRE DE CALCULS INUTILES
        do 242 , ibid = 1 , ndim
        if (zk8(ifovfd+ibid-1)(1:7) .eq. '&FOZERO') then
            fovod(ibid) = 0.d0
        else
            call fointe('FM', zk8(ifovfd+ibid-1), 1, nompar, r8bid3,&
                        fovod(ibid), iret)
        endif
242      continue
!GN        WRITE(IFM,*) 'F DUAL X   : ',ZK8(IFOVFD)
!GN        WRITE(IFM,*) 'F DUAL Y   : ',ZK8(IFOVFD+1)
    endif
!GN      WRITE(IFM,2000) 'IFOVRD', IFOVRD
!GN      WRITE(IFM,2000) 'IFOVFD', IFOVRD
!
! 2.5. --- CALCUL DU TERME D'ERREUR AVEC INTEGRATION DE GAUSS ---
!
    terpl1 = 0.d0
    termo1 = 0.d0
!
    do 25 , ipg = 1 , npg
!
! ------- CALCUL DES DERIVEES DES FONCTIONS DE FORMES /X ET /Y ---------
!
    iaux = ipg
    call dfdm2d(nno, iaux, ipoids, idfde, zr(igeom),&
                dfdx, dfdy, poids)
!
! ------- CALCUL DE L'ORIENTATION DE LA MAILLE -------------------------
!
    call utjac(.true., zr(igeom), iaux, idfde, 0,&
               ibid, nno, orien)
!
! ------- CALCUL DE LA DIVERGENCE ET DE LA NORME DE SIGMA PB. PRIMAL ---
!
    iaux=ivf+(ipg-1)*nno
    ibid = 1
    call ermev2(nno, igeom, zr(iaux), zr(iadp), nbcmp,&
                dfdx, dfdy, poids, ibid, dspx,&
                dspy, r8bid)
!
! ------- CALCUL DE LA DIVERGENCE ET DE LA NORME DE SIGMA PB. DUAL -----
!
    ibid = 0
    call ermev2(nno, igeom, zr(iaux), zr(iadd), nbcmp,&
                dfdx, dfdy, poids, ibid, dsdx,&
                dsdy, r8bid)
!
! ------- CUMUL
!
    r8bid3(1) = fppx + frpx(ipg) + dspx
    r8bid3(2) = fppy + frpy(ipg) + dspy
!
    r8bid4(1) = fpdx + frdx(ipg) + dsdx
    r8bid4(2) = fpdy + frdy(ipg) + dsdy
!
! ------- PRISE EN COMPTE DE L'EFFORT VOLUMIQUE PRIMAL EVENTUEL --------
!
    if (ifovrp .ne. 0 .or. ifovfp .ne. 0) then
!
!GN          WRITE(IFM,1000) 'F PRIMAL X', FOVOP(1)
!GN          WRITE(IFM,1000) 'F PRIMAL Y', FOVOP(2)
        r8bid3(1) = r8bid3(1) + fovop(1)
        r8bid3(2) = r8bid3(2) + fovop(2)
!
    endif
!
! ------- PRISE EN COMPTE DE L'EFFORT VOLUMIQUE DUAL EVENTUEL ----------
!
    if (ifovrd .ne. 0 .or. ifovfd .ne. 0) then
!
!GN          WRITE(IFM,1000) 'F DUAL X  ',FOVOD(1)
!GN          WRITE(IFM,1000) 'F DUAL Y  ',FOVOD(2)
        r8bid4(1) = r8bid4(1) + fovod(1)
        r8bid4(2) = r8bid4(2) + fovod(2)
!
    endif
!
! ------- CUMUL DU TERME D'ERREUR
!
    terpl1 = terpl1 + ((s*r8bid3(1)+unsurs*r8bid4(1))**2 + (s*r8bid3(2)+unsurs*r8bid4(2) )**2&
             ) * poids
!
    termo1 = termo1 + ((s*r8bid3(1)-unsurs*r8bid4(1))**2 + (s*r8bid3(2)-unsurs*r8bid4(2) )**2&
             ) * poids
    if (niv .ge. 2) then
        write(ifm,1000) 'POIDS', poids
        write(ifm,1000) 'A2 + B2', (s*r8bid3(1)+unsurs*r8bid4(1))&
            **2 + (s*r8bid3(2)+unsurs*r8bid4(2))**2
        write(ifm,1000) '==> TERPL1    ', terpl1
        write(ifm,1000) 'A2 + B2', (s*r8bid3(1)-unsurs*r8bid4(1))&
            **2 + (s*r8bid3(2)-unsurs*r8bid4(2))**2
        write(ifm,1000) '==> TERMO1    ', termo1
    endif
!
    25 end do
!
    terpl1=(hk**2)*abs(terpl1)
    termo1=(hk**2)*abs(termo1)
!GN            WRITE(IFM,*) TERPL1,TERMO1
!
! ----------------------------------------------------------------------
! ------------ FIN DU CALCUL DU PREMIER TERME DE L'ERREUR --------------
! ----------------------------------------------------------------------
!
! ----------------------------------------------------------------------
! 3. ------- CALCUL DES DEUXIEME ET TROISIEME TERMES DE L'ERREUR -------
! ----------------------------------------------------------------------
!
! 3.1. ---- INFORMATIONS SUR LA MAILLE COURANTE : ----------------------
!       TYMVOL : TYPE DE LA MAILLE VOLUMIQUE
!       NDEGRE : DEGRE DE L'ELEMENT
!       NBF    : NOMBRE DE FACES DE LA MAILLE VOLUMIQUE
!       ELREFF : DENOMINATION DE LA MAILLE FACE DE ELREFE - FAMILLE 1
!       ELREFB : DENOMINATION DE LA MAILLE FACE DE ELREFE - FAMILLE 2
!      --- REMARQUE : ON IMPOSE UNE FAMILLE DE POINTS DE GAUSS
!
    call elref7(elrefe, tymvol, ndegre, nbf, elreff,&
                elrefb)
!GN      WRITE(6,*) 'TYPE MAILLE VOLUMIQUE COURANTE :',TYMVOL
! --- CARACTERISTIQUES DES FACES DE BORD -------------------------------
!     ON EST TENTE DE FAIRE L'APPEL A ELREF4 COMME EN 3D MAIS C'EST EN
!     FAIT INUTILE CAR ON N'A BESOIN QUE DE NNOF ET NPGF.
!     CELA TOMBE BIEN CAR L'APPEL MARCHE RAREMENT ...
!
    if (ndegre .eq. 1) then
        nnof = 2
    else
        nnof = 3
    endif
!GN      CALL ELREF4 ( ELREFF, 'RIGI',
!GN     >              NDIMF, NNOF, NNOSF, NPGF, IPOIDF, IVFF,
!GN     >              IDFDXF, JGANOF )
!GN      WRITE(IFM,2000) 'NDIMF',NDIMF
!GN      WRITE(IFM,2000) 'NNOSF,NNOF,NPGF',NNOSF,NNOF,NPGF
!GN      WRITE(IFM,1000) 'IPOIDF', (ZR(IPOIDF+IFA),IFA=0,NPGF-1)
!
! 3.2. --- BOUCLE SUR LES FACES DE LA MAILLE VOLUMIQUE --------------
!
    terpl2 = 0.d0
    termo2 = 0.d0
    terpl3 = 0.d0
    termo3 = 0.d0
    do 320 , ifa = 1 , nbf
!
! ------TEST DU TYPE DE VOISIN -----------------------------------------
!
    tyv=zi(ivois+7+ifa)
!
    if (tyv .ne. 0) then
!
! ------- RECUPERATION DU TYPE DE LA MAILLE VOISINE
!
        call jenuno(jexnum('&CATA.TM.NOMTM', tyv), typmav)
        if (niv .ge. 2) then
            write(ifm,1003) ifa, zi(ivois+ifa), typmav
            1003 format (i2,'-EME FACE DE NUMERO',i10,' ==> TYPMAV = ', a)
        endif
!
! ----- CALCUL DE NORMALES, TANGENTES ET JACOBIENS AUX POINTS DE GAUSS
!
        iaux = ifa
        call calnor('2D', zr(igeom), iaux, nnos, nnof,&
                    orien, ibid, ibid, noe, ibid,&
                    ibid, ibid, jaco, nx, ny,&
                    nz, tx, ty, hf)
!
! ----------------------------------------------------------------------
! --------------- CALCUL DU DEUXIEME TERME DE L'ERREUR -----------------
! --------------- LE BORD VOISIN EST UN VOLUME -------------------------
! ----------------------------------------------------------------------
!
        if (typmav(1:4) .eq. 'TRIA' .or. typmav(1:4) .eq. 'QUAD') then
!
! ------- CALCUL DU SAUT DE CONTRAINTE ENTRE ELEMENTS ------------------
!
            iaux = ifa
            call ermes2(iaux, elrefe, typmav, irefp1, ivois,&
                        iadp, nbcmp, sgp11, sgp22, sgp12)
!
! ------- CALCUL DU SAUT DE CONTRAINTE ENTRE ELEMENTS PB. DUAL ---------
!
            call ermes2(iaux, elrefe, typmav, irefd1, ivois,&
                        iadd, nbcmp, sgd11, sgd22, sgd12)
!
! ------- CALCUL DE L'INTEGRALE SUR LA FACE ----------------------------
! ------- CALCUL DU TERME D'ERREUR AVEC INTEGRATION DE NEWTON-COTES ----
! ------- ATTENTION : CELA MARCHE CAR ON A CHOISI LA FAMILLE -----------
! ------- AVEC LES POINTS DE GAUSS SUR LES NOEUDS ----------------------
!
            do 321 , in = 1 , nnof
!
            sopl11(in)=s*sgp11(in)+unsurs*sgd11(in)
            sopl22(in)=s*sgp22(in)+unsurs*sgd22(in)
            sopl12(in)=s*sgp12(in)+unsurs*sgd12(in)
!
            somo11(in)=s*sgp11(in)-unsurs*sgd11(in)
            somo22(in)=s*sgp22(in)-unsurs*sgd22(in)
            somo12(in)=s*sgp12(in)-unsurs*sgd12(in)
!
            chplx(in) = 0.d0
            chply(in) = 0.d0
            chmox(in) = 0.d0
            chmoy(in) = 0.d0
!
321          continue
!
            call intenc(nnof, jaco, chplx, chply, sopl11,&
                        sopl22, sopl12, nx, ny, intpl)
!
            call intenc(nnof, jaco, chmox, chmoy, somo11,&
                        somo22, somo12, nx, ny, intmo)
!
! ------- CALCUL DU TERME D'ERREUR -------------------------------------
!
            if ((intpl.lt.0.d0) .or. (intmo.lt.0.d0)) then
                call utmess('A', 'INDICATEUR_9', nk=2, valk=valk)
                goto 9999
            endif
!
            terpl2=terpl2+0.5d0*hf*abs(intpl)
            termo2=termo2+0.5d0*hf*abs(intmo)
            if (niv .ge. 2) then
                write(ifm,1000) 'VOLU INTPL', intpl
                write(ifm,1000) '==> TERPL2', terpl2
                write(ifm,1000) 'VOLU INTMO', intmo
                write(ifm,1000) '==> TERMO2', termo2
            endif
!
! ----------------------------------------------------------------------
! --------------- CALCUL DU TROISIEME TERME DE L'ERREUR ----------------
! --------------- LE BORD VOISIN EST UNE FACE --------------------------
! ----------------------------------------------------------------------
!
        else if (typmav(1:3).eq.'SEG') then
!
! ------- CALCUL EFFORTS SURFACIQUES ET DES CONTRAINTES PB. PRIMAL -----
!
            iaux = ifa
            call ermeb2(iaux, irefp1, irefp2, ivois, igeom,&
                        iadp, elrefe, nbcmp, inst, nx,&
                        ny, tx, ty, sigp11, sigp22,&
                        sigp12, chpx, chpy)
!
! ------- CALCUL EFFORTS SURFACIQUES ET DES CONTRAINTES PB. DUAL -------
!
            iaux = ifa
            call ermeb2(iaux, irefd1, irefd2, ivois, igeom,&
                        iadd, elrefe, nbcmp, inst, nx,&
                        ny, tx, ty, sigd11, sigd22,&
                        sigd12, chdx, chdy)
!
! ------- CALCUL EFFORTS SURFACIQUES ET DES CONTRAINTES PB. GLOBAL -----
!
            do 322 , in = 1 , nnof
!
            chplx(in)=s*chpx(in)+unsurs*chdx(in)
            chply(in)=s*chpy(in)+unsurs*chdy(in)
            sipl11(in)=s*sigp11(in)+unsurs*sigd11(in)
            sipl22(in)=s*sigp22(in)+unsurs*sigd22(in)
            sipl12(in)=s*sigp12(in)+unsurs*sigd12(in)
!
            chmox(in)=s*chpx(in)-unsurs*chdx(in)
            chmoy(in)=s*chpy(in)-unsurs*chdy(in)
            simo11(in)=s*sigp11(in)-unsurs*sigd11(in)
            simo22(in)=s*sigp22(in)-unsurs*sigd22(in)
            simo12(in)=s*sigp12(in)-unsurs*sigd12(in)
!
322          continue
!
! ------- CALCUL DE L'INTEGRALE SUR LE BORD ----------------------------
! ------- CALCUL DU TERME D'ERREUR AVEC INTEGRATION DE NEWTON-COTES ----
!
            call intenc(nnof, jaco, chplx, chply, sipl11,&
                        sipl22, sipl12, nx, ny, intpl)
!
            call intenc(nnof, jaco, chmox, chmoy, simo11,&
                        simo22, simo12, nx, ny, intmo)
!
! ------- CALCUL DU TERME D'ERREUR -------------------------------------
!
            if ((intpl.lt.0.d0) .or. (intmo.lt.0.d0)) then
                call utmess('A', 'INDICATEUR_9', nk=2, valk=valk)
                goto 9999
            endif
!
            terpl3=terpl3+hf*abs(intpl)
            termo3=termo3+hf*abs(intmo)
            if (niv .ge. 2) then
                write(ifm,1000) 'SURF INTPL', intpl
                write(ifm,1000) '==> TERPL3', terpl3
                write(ifm,1000) 'SURF INTMO', intmo
                write(ifm,1000) '==> TERMO3', termo3
            endif
!
! ----------------------------------------------------------------------
! --------------- CURIEUX ----------------------------------------------
! ----------------------------------------------------------------------
!
        else
!
            valk(1)=typmav(1:4)
            call utmess('F', 'INDICATEUR_10', sk=valk(1))
!
        endif
!
    endif
!
    320 end do
!
! ----------------------------------------------------------------------
! ------- FIN DU CALCUL DU DEUXIEME ET TROISIEME TERME DE L'ERREUR -----
! ----------------------------------------------------------------------
!
! ----------------------------------------------------------------------
! 4. ------- MISE EN MEMOIRE DES DIFFERENTS TERMES DE L'ERREUR ---------
! ----------------------------------------------------------------------
!
    if (ndegre .eq. 2) then
        coeff=sqrt(96.d0)
    else
        coeff=sqrt(24.d0)
    endif
!
    nuplus=sqrt(terpl1+terpl2+terpl3)
    numoin=sqrt(termo1+termo2+termo3)
    errest=(1.d0/4.d0)*(nuplus-numoin)/coeff
!
    zr(ierr)=errest
!
    errest=(1.d0/4.d0)*(sqrt(terpl1)-sqrt(termo1))/coeff
!
    zr(ierr+3)=errest
!
    errest=(1.d0/4.d0)*(sqrt(terpl3)-sqrt(termo3))/coeff
!
    zr(ierr+5)=errest
!
    errest=(1.d0/4.d0)*(sqrt(terpl2)-sqrt(termo2))/coeff
!
    zr(ierr+7)=errest
!       DIAMETRE
    zr(ierr+9)=hk
!
9999  continue
!
    call jedema()
!
end subroutine
