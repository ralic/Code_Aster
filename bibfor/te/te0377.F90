subroutine te0377(option, nomte)
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
!       METHODE DES RESIDUS EXPLICITES.
!       OPTION : 'ERME_ELEM'
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
#include "asterfort/dfdm2d.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
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
    integer :: ibid, iaux, iret, itab(7), noe(9, 6, 4)
    integer :: igeom, jtime
    integer :: ierr, ivois
    integer :: imate
    integer :: iad
    integer :: ifovr, ifovf
    integer :: ipes, irot
    integer :: iref1, iref2
    integer :: ndim
    integer :: nno, nnos, npg, ipoids, ivf, idfde, jgano
    integer :: nnof, npgf
    integer :: nbcmp
    integer :: ipg
    integer :: ipgf
    integer :: nbf
    integer :: tymvol, ndegre, ifa, tyv
!
    real(kind=8) :: r8bid3(3)
    real(kind=8) :: dfdx(9), dfdy(9), hk, poids
    real(kind=8) :: fpx, fpy
    real(kind=8) :: frx(9), fry(9)
    real(kind=8) :: fovo(2)
    real(kind=8) :: dsx, dsy
    real(kind=8) :: errest, nor, norsig, sigcal, nuest, coeff
    real(kind=8) :: ter1, ter2, ter3, hf, inte, inst
    real(kind=8) :: nx(9), ny(9), nz(9), jaco(9), orien
    real(kind=8) :: chx(3), chy(3)
    real(kind=8) :: sg11(3), sg22(3), sg12(3)
    real(kind=8) :: tx(3), ty(3)
    real(kind=8) :: sig11(3), sig22(3), sig12(3)
    real(kind=8) :: e, nu, rho, valres(3)
!
    integer :: icodre(2)
    character(len=3) :: typnor
    character(len=8) :: typmav, elrefe
    character(len=8) :: elreff, elrefb
    character(len=8) :: nompar(3)
    character(len=16) :: phenom
    character(len=24) :: valk(2)
!
    aster_logical :: yapr, yaro
!
! ----------------------------------------------------------------------
! ----- NORME CALCULEE : SEMI-H1 (H1) ou ENERGIE (NRJ) -----------------
! ----------------------------------------------------------------------
!
    data typnor / 'NRJ' /
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
    call tecach('OOO', 'PCONTNO', 'L', iret, nval=3,&
                itab=itab)
    iad=itab(1)
    nbcmp=itab(2)/nno
!
! 1.4. --- CARTES DE PESANTEUR ET ROTATION
!
    call tecach('ONN', 'PPESANR', 'L', iret, iad=itab(1))
    if (itab(1) .ne. 0) then
        call jevech('PPESANR', 'L', ipes)
        yapr = .true.
    else
        yapr = .false.
    endif
    call tecach('ONN', 'PROTATR', 'L', iret, iad=itab(1))
    if (itab(1) .ne. 0) then
        call jevech('PROTATR', 'L', irot)
        yaro = .true.
    else
        yaro = .false.
    endif
!
! 1.5. --- FORCES VOLUMIQUES EVENTUELLES
!          VALEURS REELLES ?
    call tecach('ONN', 'PFRVOLU', 'L', iret, iad=ifovr)
!          OU FONCTIONS ?
    if (ifovr .eq. 0) then
        call tecach('ONN', 'PFFVOLU', 'L', iret, iad=ifovf)
    else
        ifovf = 0
    endif
!GN      WRITE(IFM,2000) 'IFOVR', IFOVR
!GN      WRITE(IFM,2000) 'IFOVF', IFOVF
!
! 1.6. --- FORCES ET PRESSIONS AUX BORDS
!
    call jevech('PFORCE', 'L', iref1)
!
    call jevech('PPRESS', 'L', iref2)
!
! 1.7. --- MATERIAU SI BESOIN
!
    if (yapr .or. yaro .or. typnor .eq. 'NRJ') then
!
        call jevech('PMATERC', 'L', imate)
        call rccoma(zi(imate), 'ELAS', 1, phenom, icodre(1))
        ibid = 0
        if (typnor .eq. 'NRJ') then
            ibid = ibid +1
            nompar(ibid) = 'E'
            ibid = ibid +1
            nompar(ibid) = 'NU'
        endif
        if (yapr .or. yaro) then
            ibid = ibid +1
            nompar(ibid) = 'RHO'
        endif
!
        call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                    ' ', phenom, 0, ' ', [0.d0],&
                    ibid, nompar, valres, icodre, 1)
!
        if (typnor .eq. 'NRJ') then
            e = valres(1)
            nu = valres(2)
        endif
        if (yapr .or. yaro) then
            rho = valres(ibid)
        endif
!GN        WRITE(IFM,1000) 'RHO, E, NU', RHO, E, NU
!
    endif
!
! ----------------------------------------------------------------------
! 2 -------------- CALCUL DU PREMIER TERME DE L'ERREUR -----------------
! ----------------------------------------------------------------------
!
! 2.1. --- CALCUL DU DIAMETRE HK DE LA MAILLE ----
!
    call uthk(nomte, zr(igeom), hk, ndim, niv)
!
! 2.2. --- CALCUL DE LA FORCE DE PESANTEUR ---
!
    if (yapr) then
        fpx=rho*zr(ipes)*zr(ipes+1)
        fpy=rho*zr(ipes)*zr(ipes+2)
    else
        fpx=0.d0
        fpy=0.d0
    endif
!GN      WRITE(IFM,1000) 'P',FPX,FPY,FPZ
!
! 2.3. --- CALCUL DE LA FORCE DE ROTATION ---
!
    if (yaro) then
        call resrot(zr(irot), zr(igeom), zr(ivf), rho, nno,&
                    npg, frx, fry)
    else
        do 23 , ipg = 1 , npg
        frx(ipg) = 0.d0
        fry(ipg) = 0.d0
 23     continue
    endif
!GN      WRITE(IFM,1000) 'R X',(FRX(IPG),IPG = 1 , NPG)
!GN      WRITE(IFM,1000) 'R Y',(FRY(IPG),IPG = 1 , NPG)
!
! 2.4. --- CALCUL DE LA FORCE VOLUMIQUE EVENTUELLE ---
!
    if (ifovr .ne. 0) then
        fovo(1) = zr(ifovr )
        fovo(2) = zr(ifovr+1)
!
    else if (ifovf.ne.0) then
        nompar(1) = 'INST'
        r8bid3(1) = inst
!       SI UNE COMPOSANTE N'A PAS ETE DECRITE, ASTER AURA MIS PAR
!       DEFAUT LA FONCTION NULLE &FOZERO. ON LE REPERE POUR
!       IMPOSER LA VALEUR 0 SANS FAIRE DE CALCULS INUTILES
        do 24 , ibid = 1 , ndim
        if (zk8(ifovf+ibid-1)(1:7) .eq. '&FOZERO') then
            fovo(ibid) = 0.d0
        else
            call fointe('FM', zk8(ifovf+ibid-1), 1, nompar, r8bid3,&
                        fovo(ibid), iret)
        endif
 24     continue
!GN        WRITE(IFM,*) 'F X : ',ZK8(IFOVF),FOVO(1)
!GN        WRITE(IFM,*) 'F Y : ',ZK8(IFOVF+1),FOVO(2)
    endif
!
! 2.5. --- CALCUL DU TERME D'ERREUR AVEC INTEGRATION DE GAUSS ---
!
    ter1 = 0.d0
    norsig = 0.d0
!
    do 25 , ipg = 1 , npg
!
! ------- CALCUL DES DERIVEES DES FONCTIONS DE FORMES /X ET /Y ---------
!
    call dfdm2d(nno, ipg, ipoids, idfde, zr(igeom),&
                poids, dfdx, dfdy)
!
! ------- CALCUL DE L'ORIENTATION DE LA MAILLE -------------------------
!
    call utjac(.true._1, zr(igeom), ipg, idfde, 0,&
               ibid, nno, orien)
!
! ------- CALCUL DE LA DIVERGENCE ET DE LA NORME DE SIGMA --------------
!
    iaux=ivf+(ipg-1)*nno
    ibid = 1
    call ermev2(nno, igeom, zr(iaux), zr(iad), nbcmp,&
                dfdx, dfdy, poids, ibid, dsx,&
                dsy, nor)
!
! ------- CUMUL
!
    r8bid3(1) = fpx + frx(ipg) + dsx
    r8bid3(2) = fpy + fry(ipg) + dsy
!
! ------- PRISE EN COMPTE DE L'EFFORT VOLUMIQUE EVENTUEL ---------------
!
    if (ifovr .ne. 0 .or. ifovf .ne. 0) then
!
!GN          WRITE(IFM,1000) 'F X', FOVO(1)
!GN          WRITE(IFM,1000) 'F Y', FOVO(2)
        r8bid3(1) = r8bid3(1) + fovo(1)
        r8bid3(2) = r8bid3(2) + fovo(2)
!
    endif
!
! ------- CUMUL DU TERME D'ERREUR
!
    ter1 = ter1 + ( r8bid3(1)**2 + r8bid3(2)**2 ) * poids
    if (niv .ge. 2) then
        write(ifm,1000) 'POIDS', poids
        write(ifm,1000) 'A2 + B2 ', r8bid3(1)**2 + r8bid3(2)**2
        write(ifm,1000) '==> TER1', ter1
    endif
!
! ------- CALCUL DE LA NORME DE SIGMA SUR L'ELEMENT --------------------
!
    norsig = norsig + nor*poids
!
    25 end do
!
    if (typnor(1:2) .eq. 'H1') then
!       NORME H1
        ter1=hk*sqrt(ter1)
    else if (typnor.eq.'NRJ') then
!       NORME EN ENERGIE
        ter1=(hk**2)*abs(ter1)
    endif
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
!     on est tente de faire l'appel a elrefe_info comme en 3d mais c'est en
!     FAIT INUTILE CAR ON N'A BESOIN QUE DE NNOF ET NPGF.
!     CELA TOMBE BIEN CAR L'APPEL MARCHE RAREMENT ...
!
    if (ndegre .eq. 1) then
        nnof = 2
    else
        nnof = 3
    endif
    npgf = nnof
!GN      CALL ELREF4 ( ELREFF,fami='RIGI',
!GN     >              NDIMF, NNOF, NNOSF, NPGF, IPOIDF, IVFF,
!GN     >              IDFDXF, JGANOF )
!GN      WRITE(IFM,2000) 'NDIMF',NDIMF
!GN      WRITE(IFM,2000) 'NNOSF,NNOF,NPGF',NNOSF,NNOF,NPGF
!GN      WRITE(IFM,1000) 'IPOIDF', (ZR(IPOIDF+IFA),IFA=0,NPGF-1)
!
! 3.2. --- BOUCLE SUR LES FACES DE LA MAILLE VOLUMIQUE --------------
!
    ter2 = 0.d0
    ter3 = 0.d0
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
            call ermes2(iaux, elrefe, typmav, iref1, ivois,&
                        iad, nbcmp, sg11, sg22, sg12)
!
! ------- CALCUL DE L'INTEGRALE SUR LA FACE ----------------------------
! ------- CALCUL DU TERME D'ERREUR AVEC INTEGRATION DE NEWTON-COTES ----
! ------- ATTENTION : CELA MARCHE CAR ON A CHOISI LA FAMILLE -----------
! ------- AVEC LES POINTS DE GAUSS SUR LES NOEUDS ----------------------
!
            do 321 , ipgf = 1 , npgf
            chx(ipgf) = 0.d0
            chy(ipgf) = 0.d0
321         continue
!
            call intenc(nnof, jaco, chx, chy, sg11,&
                        sg22, sg12, nx, ny, inte)
!
! ------- CALCUL DU TERME D'ERREUR -------------------------------------
!
            if (inte .lt. 0.d0) then
                call utmess('A', 'INDICATEUR_9', nk=2, valk=valk)
                goto 999
            endif
!
            if (typnor(1:2) .eq. 'H1') then
!             NORME H1
                ter2=ter2+0.5d0*sqrt(hf)*sqrt(inte)
            else if (typnor.eq.'NRJ') then
!             NORME EN ENERGIE
                ter2=ter2+0.5d0*hf*inte
            endif
            if (niv .ge. 2) then
                write(ifm,1000) 'VOLU INTE', inte
                write(ifm,1000) '==> TER2 ', ter2
            endif
!
! ----------------------------------------------------------------------
! --------------- CALCUL DU TROISIEME TERME DE L'ERREUR ----------------
! --------------- LE BORD VOISIN EST UNE FACE --------------------------
! ----------------------------------------------------------------------
!
        else if (typmav(1:3).eq.'SEG') then
!
! ------- CALCUL EFFORTS SURFACIQUES ET DES CONTRAINTES ----------------
!
            iaux = ifa
            call ermeb2(iaux, iref1, iref2, ivois, igeom,&
                        iad, elrefe, nbcmp, inst, nx,&
                        ny, tx, ty, sig11, sig22,&
                        sig12, chx, chy)
!
! ------- CALCUL DE L'INTEGRALE SUR LE BORD ----------------------------
!
            call intenc(nnof, jaco, chx, chy, sig11,&
                        sig22, sig12, nx, ny, inte)
!
! ------- CALCUL DU TERME D'ERREUR -------------------------------------
!
            if (inte .lt. 0.d0) then
                call utmess('A', 'INDICATEUR_9', nk=2, valk=valk)
                goto 999
            endif
!
            if (typnor(1:2) .eq. 'H1') then
!             NORME H1
                ter3=ter3+sqrt(hf)*sqrt(inte)
            else if (typnor.eq.'NRJ') then
!             NORME EN ENERGIE
                ter3=ter3+hf*inte
            endif
            if (niv .ge. 2) then
                write(ifm,1000) 'SURF INTE', inte
                write(ifm,1000) '==> TER3 ', ter3
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
    if (typnor(1:2) .eq. 'H1') then
!
        if (ndegre .eq. 2) then
            coeff=sqrt(96.d0)
        else
            coeff=sqrt(24.d0)
        endif
!
!      NORME H1
        errest=(ter1+ter2+ter3)/coeff
        sigcal=sqrt(norsig)
        if ((errest**2+norsig) .ne. 0.d0) then
            nuest=100.d0*sqrt(errest**2/(errest**2+norsig))
        else
            nuest=0.d0
        endif
!
        zr(ierr)=errest
        zr(ierr+1)=nuest
        zr(ierr+2)=sigcal
!
        errest=ter1/coeff
        if ((errest**2+norsig) .ne. 0.d0) then
            nuest=100.d0*sqrt(errest**2/(errest**2+norsig))
        else
            nuest=0.d0
        endif
!
        zr(ierr+3)=errest
        zr(ierr+4)=nuest
!
        errest=ter3/coeff
        if ((errest**2+norsig) .ne. 0.d0) then
            nuest=100.d0*sqrt(errest**2/(errest**2+norsig))
        else
            nuest=0.d0
        endif
!
        zr(ierr+5)=errest
        zr(ierr+6)=nuest
!
        errest=ter2/coeff
        if ((errest**2+norsig) .ne. 0.d0) then
            nuest=100.d0*sqrt(errest**2/(errest**2+norsig))
        else
            nuest=0.d0
        endif
!
        zr(ierr+7)=errest
        zr(ierr+8)=nuest
!
    else if (typnor.eq.'NRJ') then
!
        if (ndegre .eq. 2) then
            coeff=sqrt(96.d0*e/(1-nu))
        else
            coeff=sqrt(24.d0*e/(1-nu))
        endif
!
!      NORME EN ENERGIE
        errest=sqrt(ter1+ter2+ter3)/coeff
        sigcal=sqrt(norsig)
        if ((errest**2+norsig) .ne. 0.d0) then
            nuest=100.d0*sqrt(errest**2/(errest**2+norsig))
        else
            nuest=0.d0
        endif
!
        zr(ierr)=errest
        zr(ierr+1)=nuest
        zr(ierr+2)=sigcal
!
        errest=sqrt(ter1)/coeff
        if ((errest**2+norsig) .ne. 0.d0) then
            nuest=100.d0*sqrt(errest**2/(errest**2+norsig))
        else
            nuest=0.d0
        endif
!
        zr(ierr+3)=errest
        zr(ierr+4)=nuest
!
        errest=sqrt(ter3)/coeff
        if ((errest**2+norsig) .ne. 0.d0) then
            nuest=100.d0*sqrt(errest**2/(errest**2+norsig))
        else
            nuest=0.d0
        endif
!
        zr(ierr+5)=errest
        zr(ierr+6)=nuest
!
        errest=sqrt(ter2)/coeff
        if ((errest**2+norsig) .ne. 0.d0) then
            nuest=100.d0*sqrt(errest**2/(errest**2+norsig))
        else
            nuest=0.d0
        endif
!
        zr(ierr+7)=errest
        zr(ierr+8)=nuest
!
    endif
!       DIAMETRE
    zr(ierr+9)=hk
!
999 continue
!
    call jedema()
!
end subroutine
