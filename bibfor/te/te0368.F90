subroutine te0368(option, nomte)
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
!       CALCUL DE L'INDICATEUR D'ERREUR EN MECANIQUE 3D AVEC LA
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/calnor.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/elref7.h"
#include "asterfort/ermeb3.h"
#include "asterfort/ermes3.h"
#include "asterfort/ermev3.h"
#include "asterfort/fointe.h"
#include "asterfort/infniv.h"
#include "asterfort/intega.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jevech.h"
#include "asterfort/jexnum.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/resr3d.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/uthk.h"
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
    integer :: noe(9, 6, 4)
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
    integer :: ndimf
    integer :: nnof, nnosf, npgf, ipoidf, ivff, idfdxf, jganof
    integer :: nno2, nnos2, npg2, ipoid2, ivf2, idfdx2, jgano2
    integer :: nbcmp
    integer :: ipg, in
    integer :: nbf
    integer :: tymvol, ndegre, ifa, tyv
!
    real(kind=8) :: r8bid, r8bid2, r8bid3(3), r8bid4(3)
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), hk, poids
    real(kind=8) :: fppx, fppy, fppz
    real(kind=8) :: fpdx, fpdy, fpdz
    real(kind=8) :: frpx(27), frpy(27), frpz(27)
    real(kind=8) :: frdx(27), frdy(27), frdz(27)
    real(kind=8) :: fovop(3)
    real(kind=8) :: fovod(3)
    real(kind=8) :: dspx, dspy, dspz
    real(kind=8) :: dsdx, dsdy, dsdz
    real(kind=8) :: errest, coeff
    real(kind=8) :: terpl1, termo1, terpl2, termo2, terpl3, termo3
    real(kind=8) :: hf, intpl, intmo, inst
    real(kind=8) :: s, unsurs
    real(kind=8) :: nx(9), ny(9), nz(9), jaco(9)
    real(kind=8) :: chpx(9), chpy(9), chpz(9), chdx(9), chdy(9), chdz(9)
    real(kind=8) :: sgp11(9), sgp22(9), sgp33(9), sgp12(9), sgp13(9), sgp23(9)
    real(kind=8) :: sgd11(9), sgd22(9), sgd33(9), sgd12(9), sgd13(9), sgd23(9)
    real(kind=8) :: sigp11(9), sigp22(9), sigp33(9), sigp12(9), sigp13(9)
    real(kind=8) :: sigp23(9)
    real(kind=8) :: sigd11(9), sigd22(9), sigd33(9), sigd12(9), sigd13(9)
    real(kind=8) :: sigd23(9)
    real(kind=8) :: chplx(9), chply(9), chplz(9), chmox(9), chmoy(9), chmoz(9)
    real(kind=8) :: sopl11(9), sopl22(9), sopl33(9), sopl12(9), sopl13(9)
    real(kind=8) :: sopl23(9)
    real(kind=8) :: somo11(9), somo22(9), somo33(9), somo12(9), somo13(9)
    real(kind=8) :: somo23(9)
    real(kind=8) :: sipl11(9), sipl22(9), sipl33(9), sipl12(9), sipl13(9)
    real(kind=8) :: sipl23(9)
    real(kind=8) :: simo11(9), simo22(9), simo33(9), simo12(9), simo13(9)
    real(kind=8) :: simo23(9)
    real(kind=8) :: nuplus, numoin
    real(kind=8) :: rho, valres(1)
!
    integer :: icodre(2), kpg, spt
    character(len=8) :: fami, poum
    character(len=8) :: typmav, elrefe
    character(len=8) :: elreff, elrefb
    character(len=8) :: nompar(1)
    character(len=16) :: phenom
    character(len=24) :: valk(2)
!
    aster_logical :: yaprp, yarop
    aster_logical :: yaprd, yarod
!
! --- INITIALISATION DU TABLEAU DES NUMEROS DE NOEUDS FACE PAR FACE ----
!
!     NOE (IN,IFA,TYMVOL) : IN     : NUMERO DU NOEUD DANS LA FACE
!                           IFA    : NUMERO DE LA FACE
!                           TYMVOL : TYPE DE LA MAILLE VOLUMIQUE
!                                    1 : HEXAEDRE A 8,20 ET 27 NOEUDS
!                                    2 : PENTAEDRE A 6,15 ET 18 NOEUDS
!                                    3 : TETRAEDRE A 4 ET 10 NOEUDS
!                                    4 : PYRAMIDE A 5 ET 13 NOEUDS
!     VOIR TE003 POUR LES EXPLICATIONS DETAILLEES
!
    data noe/1,4,3,2,12,11,10, 9,21, 1,2,6,5, 9,14,17,13,22,&
     &         2,3,7,6,10,15,18,14,23, 3,4,8,7,11,16,19,15,24,&
     &         4,1,5,8,12,13,20,16,25, 5,6,7,8,17,18,19,20,26,&
     &         1,3,2,9,8,7, 3*0,       4,5,6,13,14,15, 3*0,&
     &         1,2,5,4, 7,11,13,10,16, 2,3,6,5,8,12,14,11,17,&
     &         1,4,6,3,10,15,12, 9,18, 9*0,&
     &         1,3,2,7,6, 5, 3*0,      2,3,4,6,10,9, 3*0,&
     &         3,1,4,7,8,10, 3*0,      1,2,4,5, 9,8, 3*0,&
     &         9*0,                    9*0,&
     &         1,2,5,6,11,10, 3*0,     2,3,5,7,12,11, 3*0,&
     &         3,4,5,8,13,12, 3*0,     4,1,5,9,10,13, 3*0,&
     &         1,4,3,2,9,8,7,6, 0,     9*0 /
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
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfde, jgano=jgano)
!
! 1.3. --- CHAMP DE CONTRAINTES
!
    call tecach('OOO', 'PCONTNOP', 'L', iret, nval=3,&
                itab=itab)
    iadp=itab(1)
    nbcmp=itab(2)/nno
    call tecach('OOO', 'PCONTNOD', 'L', iret, nval=3,&
                itab=itab)
    iadd=itab(1)
!
! 1.4. --- CARTES DE PESANTEUR ET ROTATION
!
    call tecach('ONN', 'PPESANRP', 'L', iret, iad=itab(1))
    if (itab(1) .ne. 0) then
        call jevech('PPESANRP', 'L', ipesp)
        yaprp = .true.
    else
        yaprp = .false.
    endif
    call tecach('ONN', 'PROTATRP', 'L', iret, iad=itab(1))
    if (itab(1) .ne. 0) then
        call jevech('PROTATRP', 'L', irotp)
        yarop = .true.
    else
        yarop = .false.
    endif
    call tecach('ONN', 'PPESANRD', 'L', iret, iad=itab(1))
    if (itab(1) .ne. 0) then
        call jevech('PPESANRD', 'L', ipesd)
        yaprd = .true.
    else
        yaprd = .false.
    endif
    call tecach('ONN', 'PROTATRD', 'L', iret, iad=itab(1))
    if (itab(1) .ne. 0) then
        call jevech('PROTATRD', 'L', irotd)
        yarod = .true.
    else
        yarod = .false.
    endif
!
! 1.5. --- FORCES VOLUMIQUES EVENTUELLES
!          VALEURS REELLES ?
    call tecach('ONN', 'PFRVOLUP', 'L', iret, iad=ifovrp)
!          OU FONCTIONS ?
    if (ifovrp .eq. 0) then
        call tecach('ONN', 'PFFVOLUP', 'L', iret, iad=ifovfp)
    else
        ifovfp = 0
    endif
!          VALEURS REELLES ?
    call tecach('ONN', 'PFRVOLUD', 'L', iret, iad=ifovrd)
!          OU FONCTIONS ?
    if (ifovrd .eq. 0) then
        call tecach('ONN', 'PFFVOLUD', 'L', iret, iad=ifovfd)
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
    if (yaprp .or. yarop .or. yaprd .or. yarod) then
!
        call jevech('PMATERC', 'L', imate)
        call rccoma(zi(imate), 'ELAS', 1, phenom, icodre(1))
        nompar(1)='RHO'
        fami='FPG1'
        kpg=1
        spt=1
        poum='+'
        call rcvalb(fami, kpg, spt, poum, zi(imate),&
                    ' ', phenom, 1, ' ', [r8bid],&
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
    call uthk(nomte, zr(igeom), hk, ndim, niv)
!
! 2.2. --- CALCUL DE LA FORCE DE PESANTEUR  ---
!
! ------ CALCUL DE LA FORCE DE PESANTEUR PB. PRIMAL --------------------
    if (yaprp) then
        fppx=rho*zr(ipesp)*zr(ipesp+1)
        fppy=rho*zr(ipesp)*zr(ipesp+2)
        fppz=rho*zr(ipesp)*zr(ipesp+3)
    else
        fppx=0.d0
        fppy=0.d0
        fppz=0.d0
    endif
!GN      WRITE(IFM,1000) 'P PRIMAL',FPPX,FPPY,FPPZ
! ------ CALCUL DE LA FORCE DE PESANTEUR PB. DUAL ----------------------
    if (yaprd) then
        fpdx=rho*zr(ipesd)*zr(ipesd+1)
        fpdy=rho*zr(ipesd)*zr(ipesd+2)
        fpdz=rho*zr(ipesd)*zr(ipesd+3)
    else
        fpdx=0.d0
        fpdy=0.d0
        fpdz=0.d0
    endif
!GN      WRITE(IFM,1000) 'P DUAL  ',FPDX,FPDY,FPDZ
!
! 2.3. --- CALCUL DE LA FORCE DE ROTATION ---
! ------ CALCUL DE LA FORCE DE ROTATION AUX POINTS DE GAUSS PB. PRIMAL -
!
    if (yarop) then
        call resr3d(zr(irotp), zr(igeom), zr(ivf), rho, nno,&
                    npg, frpx, frpy, frpz)
    else
        do 231 , ipg = 1 , npg
        frpx(ipg) = 0.d0
        frpy(ipg) = 0.d0
        frpz(ipg) = 0.d0
231     continue
    endif
!GN      WRITE(IFM,1000) 'R PRIMAL X',(FRPX(IPG),IPG = 1 , NPG)
!GN      WRITE(IFM,1000) 'R PRIMAL Y',(FRPY(IPG),IPG = 1 , NPG)
!GN      WRITE(IFM,1000) 'R PRIMAL Z',(FRPZ(IPG),IPG = 1 , NPG)
! ------ CALCUL DE LA FORCE DE ROTATION AUX POINTS DE GAUSS PB. DUAL ---
    if (yarod) then
        call resr3d(zr(irotd), zr(igeom), zr(ivf), rho, nno,&
                    npg, frdx, frdy, frdz)
    else
        do 232 , ipg = 1 , npg
        frdx(ipg) = 0.d0
        frdy(ipg) = 0.d0
        frdz(ipg) = 0.d0
232     continue
    endif
!GN      WRITE(IFM,1000) 'R DUAL X  ',(FRDX(IPG),IPG = 1 , NPG)
!GN      WRITE(IFM,1000) 'R DUAL Y  ',(FRDY(IPG),IPG = 1 , NPG)
!GN      WRITE(IFM,1000) 'R DUAL Z  ',(FRDZ(IPG),IPG = 1 , NPG)
!
! 2.4. --- CALCUL DE LA FORCE VOLUMIQUE EVENTUELLE ---
!
    if (ifovrp .ne. 0) then
        fovop(1) = zr(ifovrp )
        fovop(2) = zr(ifovrp+1)
        fovop(3) = zr(ifovrp+2)
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
241     continue
!GN        WRITE(IFM,*) 'F PRIMAL X : ',ZK8(IFOVFP)
!GN        WRITE(IFM,*) 'F PRIMAL Y : ',ZK8(IFOVFP+1)
!GN        WRITE(IFM,*) 'F PRIMAL Z : ',ZK8(IFOVFP+2)
    endif
!GN      WRITE(IFM,2000) 'IFOVRP', IFOVRP
!GN      WRITE(IFM,2000) 'IFOVFP', IFOVRP
!
    if (ifovrd .ne. 0) then
        fovod(1) = zr(ifovrd )
        fovod(2) = zr(ifovrd+1)
        fovod(3) = zr(ifovrd+2)
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
242     continue
!GN        WRITE(IFM,*) 'F DUAL X   : ',ZK8(IFOVFD)
!GN        WRITE(IFM,*) 'F DUAL Y   : ',ZK8(IFOVFD+1)
!GN        WRITE(IFM,*) 'F DUAL Z   : ',ZK8(IFOVFD+2)
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
! ------- CALCUL DES DERIVEES DES FONCTIONS DE FORMES /X, /Y ET /Z -----
!
    iaux = ipg
    call dfdm3d(nno, iaux, ipoids, idfde, zr(igeom),&
                poids, dfdx, dfdy, dfdz)
!
! ------- CALCUL DE LA DIVERGENCE ET DE LA NORME DE SIGMA PB. PRIMAL ---
!
    call ermev3(nno, iaux, ivf, iadp, nbcmp,&
                dfdx, dfdy, dfdz, dspx, dspy,&
                dspz, r8bid)
!
! ------- CALCUL DE LA DIVERGENCE ET DE LA NORME DE SIGMA PB. DUAL -----
!
    call ermev3(nno, iaux, ivf, iadd, nbcmp,&
                dfdx, dfdy, dfdz, dsdx, dsdy,&
                dsdz, r8bid)
!
! ------- CUMUL
!
    r8bid3(1) = fppx + frpx(ipg) + dspx
    r8bid3(2) = fppy + frpy(ipg) + dspy
    r8bid3(3) = fppz + frpz(ipg) + dspz
!
    r8bid4(1) = fpdx + frdx(ipg) + dsdx
    r8bid4(2) = fpdy + frdy(ipg) + dsdy
    r8bid4(3) = fpdz + frdz(ipg) + dsdz
!
! ------- PRISE EN COMPTE DE L'EFFORT VOLUMIQUE PRIMAL EVENTUEL --------
!
    if (ifovrp .ne. 0 .or. ifovfp .ne. 0) then
!
!GN          WRITE(IFM,1000) 'F PRIMAL X', FOVOP(1)
!GN          WRITE(IFM,1000) 'F PRIMAL Y', FOVOP(2)
!GN          WRITE(IFM,1000) 'F PRIMAL Z', FOVOP(3)
        r8bid3(1) = r8bid3(1) + fovop(1)
        r8bid3(2) = r8bid3(2) + fovop(2)
        r8bid3(3) = r8bid3(3) + fovop(3)
!
    endif
!
! ------- PRISE EN COMPTE DE L'EFFORT VOLUMIQUE DUAL EVENTUEL ----------
!
    if (ifovrd .ne. 0 .or. ifovfd .ne. 0) then
!
!GN          WRITE(IFM,1000) 'F DUAL X  ',FOVOD(1)
!GN          WRITE(IFM,1000) 'F DUAL Y  ',FOVOD(2)
!GN          WRITE(IFM,1000) 'F DUAL Z  ',FOVOD(3)
        r8bid4(1) = r8bid4(1) + fovod(1)
        r8bid4(2) = r8bid4(2) + fovod(2)
        r8bid4(3) = r8bid4(3) + fovod(3)
!
    endif
!
! ------- CUMUL DU TERME D'ERREUR
!
    terpl1 = terpl1 + (&
             (&
             s*r8bid3(1)+unsurs*r8bid4(1))**2 + (s*r8bid3(2)+unsurs*r8bid4(2))**2 + (s*r8bid3(3)+&
             &unsurs* r8bid4(3)&
             )**2&
             ) * poids
!
    termo1 = termo1 + (&
             (&
             s*r8bid3(1)-unsurs*r8bid4(1))**2 + (s*r8bid3(2)-unsurs*r8bid4(2))**2 + (s*r8bid3(3)-&
             &unsurs* r8bid4(3)&
             )**2&
             ) * poids
    if (niv .ge. 2) then
        write(ifm,1000) 'POIDS', poids
        write(ifm,1000) 'A2 + B2 + C2', (s*r8bid3(1)+unsurs*&
            r8bid4(1))**2 + (s*r8bid3(2)+unsurs*r8bid4(2))**2 +&
            (s*r8bid3(3)+unsurs*r8bid4(3))**2
        write(ifm,1000) '==> TERPL1    ', terpl1
        write(ifm,1000) 'A2 + B2 + C2', (s*r8bid3(1)-unsurs*&
            r8bid4(1))**2 + (s*r8bid3(2)-unsurs*r8bid4(2))**2 +&
            (s*r8bid3(3)-unsurs*r8bid4(3))**2
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
! --- CARACTERISTIQUES DES FACES DE BORD DE LA FAMILLE 1 ---------------
    call elrefe_info(elrefe=elreff, fami='NOEU', ndim=ndimf, nno=nnof, nnos=nnosf,&
                     npg=npgf, jpoids=ipoidf, jvf=ivff, jdfde=idfdxf, jgano=jganof)
!GN      WRITE(IFM,2000) 'NDIMF',NDIMF
!GN      WRITE(IFM,2000) 'NNOSF,NNOF,NPGF',NNOSF,NNOF,NPGF
!GN      WRITE(IFM,1000) 'IPOIDF', (ZR(IPOIDF+IFA),IFA=0,NPGF-1)
!
! --- COMPLEMENT EVENTUEL POUR LES MAILLES QUI ONT 2 TYPES DE ---
! --- MAILLES DE BORD (PENTAEDRE, PYRAMIDE) ---
!
    if (elrefb(1:1) .ne. ' ') then
        call elrefe_info(elrefe=elrefb, fami='NOEU', ndim=ndimf, nno=nno2, nnos=nnos2,&
                         npg=npg2, jpoids=ipoid2, jvf=ivf2, jdfde=idfdx2, jgano=jgano2)
!GN       WRITE(IFM,2000) 'NDIMF,NNO2',NDIMF,NNO2
!GN       WRITE(IFM,2000) 'NNOS2,NPG2',NNOS2,NPG2
!GN       WRITE(IFM,1000) 'IPOID2', (ZR(IPOID2+IFA),IFA=0,NPG2-1)
    endif
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
! --- QUAND ON ARRIVE AUX FACES QUAD DES PENTAEDRES OU DES PYRAMIDES ---
! --- IL FAUT REMPLACER LES CARACTERISTIQUES DE LA FAMILLE 1         ---
! --- PAR CELLES DE LA FAMILLE 2                                     ---
!
        if (( tymvol.eq.2 .and. ifa.ge.3 ) .or. ( tymvol.eq.4 .and. ifa.ge.5 )) then
!
            nnof = nno2
            npgf = npg2
            nnosf = nnos2
            ipoidf = ipoid2
            idfdxf = idfdx2
!
        endif
!GN      WRITE(IFM,*) '. NPGF =', NPGF
!
! ----- CALCUL DU DIAMETRE HF DE LA FACE ----------
!
        ibid=0
        call uthk(nomte, zr(igeom), hf, ibid, niv,&
                  noe, nnosf, tymvol, ifa)
!
! ------- CALCUL DE NORMALES ET JACOBIENS AUX POINTS DE GAUSS ----------
!
        iaux = ifa
        call calnor('3D', zr(igeom), ibid, ibid, ibid,&
                    r8bid, nnof, npgf, noe, iaux,&
                    tymvol, idfdxf, jaco, nx, ny,&
                    nz, r8bid3, r8bid4, r8bid2)
!
! ----------------------------------------------------------------------
! --------------- CALCUL DU DEUXIEME TERME DE L'ERREUR -----------------
! --------------- LE BORD VOISIN EST UN VOLUME -------------------------
! ----------------------------------------------------------------------
!
        if (typmav(1:4) .eq. 'HEXA' .or. typmav(1:4) .eq. 'PENT' .or. typmav(1:4) .eq.&
            'TETR' .or. typmav(1:4) .eq. 'PYRA') then
!
! ------- CALCUL DU SAUT DE CONTRAINTE ENTRE ELEMENTS PB. PRIMAL -------
! ------- CE CHAMP SGPXX EST EXPRIME SUR LES NOEUDS DE LA FACE ---------
!
            call ermes3(noe, ifa, tymvol, nnof, typmav,&
                        irefp1, ivois, iadp, nbcmp, sgp11,&
                        sgp22, sgp33, sgp12, sgp13, sgp23)
!
! ------- CALCUL DU SAUT DE CONTRAINTE ENTRE ELEMENTS PB. DUAL ---------
!
            call ermes3(noe, ifa, tymvol, nnof, typmav,&
                        irefd1, ivois, iadd, nbcmp, sgd11,&
                        sgd22, sgd33, sgd12, sgd13, sgd23)
!
! ------- CALCUL DE L'INTEGRALE SUR LA FACE ----------------------------
! ------- ATTENTION : CELA MARCHE CAR ON A CHOISI LA FAMILLE -----------
! ------- AVEC LES POINTS DE GAUSS SUR LES NOEUDS ----------------------
!
            do 321 , in = 1 , nnof
!
            sopl11(in)=s*sgp11(in)+unsurs*sgd11(in)
            sopl22(in)=s*sgp22(in)+unsurs*sgd22(in)
            sopl33(in)=s*sgp33(in)+unsurs*sgd33(in)
            sopl12(in)=s*sgp12(in)+unsurs*sgd12(in)
            sopl13(in)=s*sgp13(in)+unsurs*sgd13(in)
            sopl23(in)=s*sgp23(in)+unsurs*sgd23(in)
!
            somo11(in)=s*sgp11(in)-unsurs*sgd11(in)
            somo22(in)=s*sgp22(in)-unsurs*sgd22(in)
            somo33(in)=s*sgp33(in)-unsurs*sgd33(in)
            somo12(in)=s*sgp12(in)-unsurs*sgd12(in)
            somo13(in)=s*sgp13(in)-unsurs*sgd13(in)
            somo23(in)=s*sgp23(in)-unsurs*sgd23(in)
!
            chplx(in) = 0.d0
            chply(in) = 0.d0
            chplz(in) = 0.d0
            chmox(in) = 0.d0
            chmoy(in) = 0.d0
            chmoz(in) = 0.d0
!
321         continue
!
            call intega(npgf, jaco, zr(ipoidf), chplx, chply,&
                        chplz, sopl11, sopl22, sopl33, sopl12,&
                        sopl13, sopl23, nx, ny, nz,&
                        intpl)
!
            call intega(npgf, jaco, zr(ipoidf), chmox, chmoy,&
                        chmoz, somo11, somo22, somo33, somo12,&
                        somo13, somo23, nx, ny, nz,&
                        intmo)
!
! ------- CALCUL DU TERME D'ERREUR -------------------------------------
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
            elseif ( typmav(1:4).eq.'QUAD' .or. typmav(1:4).eq.'TRIA'&
            ) then
!
! ------- CALCUL EFFORTS SURFACIQUES ET DES CONTRAINTES PB. PRIMAL -----
!
            call ermeb3(noe, ifa, tymvol, nnof, irefp1,&
                        irefp2, ivois, igeom, iadp, nbcmp,&
                        inst, nx, ny, nz, sigp11,&
                        sigp22, sigp33, sigp12, sigp13, sigp23,&
                        chpx, chpy, chpz)
!
! ------- CALCUL EFFORTS SURFACIQUES ET DES CONTRAINTES PB. DUAL -------
!
            call ermeb3(noe, ifa, tymvol, nnof, irefd1,&
                        irefd2, ivois, igeom, iadd, nbcmp,&
                        inst, nx, ny, nz, sigd11,&
                        sigd22, sigd33, sigd12, sigd13, sigd23,&
                        chdx, chdy, chdz)
!
! ------- CALCUL EFFORTS SURFACIQUES ET DES CONTRAINTES PB. GLOBAL -----
!
            do 322 , in = 1 , nnof
!
            chplx(in)=s*chpx(in)+unsurs*chdx(in)
            chply(in)=s*chpy(in)+unsurs*chdy(in)
            chplz(in)=s*chpz(in)+unsurs*chdz(in)
            sipl11(in)=s*sigp11(in)+unsurs*sigd11(in)
            sipl22(in)=s*sigp22(in)+unsurs*sigd22(in)
            sipl33(in)=s*sigp33(in)+unsurs*sigd33(in)
            sipl12(in)=s*sigp12(in)+unsurs*sigd12(in)
            sipl13(in)=s*sigp13(in)+unsurs*sigd13(in)
            sipl23(in)=s*sigp23(in)+unsurs*sigd23(in)
!
            chmox(in)=s*chpx(in)-unsurs*chdx(in)
            chmoy(in)=s*chpy(in)-unsurs*chdy(in)
            chmoz(in)=s*chpz(in)-unsurs*chdz(in)
            simo11(in)=s*sigp11(in)-unsurs*sigd11(in)
            simo22(in)=s*sigp22(in)-unsurs*sigd22(in)
            simo33(in)=s*sigp33(in)-unsurs*sigd33(in)
            simo12(in)=s*sigp12(in)-unsurs*sigd12(in)
            simo13(in)=s*sigp13(in)-unsurs*sigd13(in)
            simo23(in)=s*sigp23(in)-unsurs*sigd23(in)
!
322         continue
!
! ------- CALCUL DE L'INTEGRALE SUR LA FACE ----------------------------
! ------- ATTENTION : CELA MARCHE CAR ON A CHOISI LA FAMILLE -----------
! ------- AVEC LES POINTS DE GAUSS SUR LES NOEUDS ----------------------
!
            call intega(npgf, jaco, zr(ipoidf), chplx, chply,&
                        chplz, sipl11, sipl22, sipl33, sipl12,&
                        sipl13, sipl23, nx, ny, nz,&
                        intpl)
!
            call intega(npgf, jaco, zr(ipoidf), chmox, chmoy,&
                        chmoz, simo11, simo22, simo33, simo12,&
                        simo13, simo23, nx, ny, nz,&
                        intmo)
!
! ------- CALCUL DU TERME D'ERREUR -------------------------------------
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
    call jedema()
!
end subroutine
