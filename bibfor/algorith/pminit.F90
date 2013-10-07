subroutine pminit(imate, nbvari, ndim, typmod, table,&
                  nbpar, iforta, nompar, typpar, ang,&
                  pgl, irota, epsm, sigm, vim,&
                  vip, vr, defimp, coef, indimp,&
                  fonimp, cimpo, kel, sddisc, parcri,&
                  pred, matrel, imptgt, option, nomvi,&
                  nbvita, nbvrcm, sderro)
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
!
! aslint: disable=W1504
    implicit none
!
!-----------------------------------------------------------------------
!     OPERATEUR    CALC_POINT_MAT : INITIALISATIONS
!-----------------------------------------------------------------------
!
! IN   IMATE  : ADRESSE MATERIAU CODE
! IN   NBVARI : NOMBRE DE VARIABLES INTERNES
! IN   NDIM   : 3
! OUT  TYPMOD : 3D
! OUT  TABLE  : TABLE RESULTAT
! OUT  NBPAR  : NOMBRE DE PARAMETRES DE LA TABLE RESULTAT
! OUT  NOMPAR : NOMS DES PARAMETRES DE LA TABLE RESULTAT
! OUT  ANG    : ANGLES DU MOT-CLE MASSIF
! OUT  PGL    : MATRICE DE ROTATION AUTOUR DE Z
! OUT  IROTA  : =1 SI ROTATION AUTOUR DE Z
! OUT  EPSM   : DEFORMATIONS INITIALES
! OUT  SIGM   : CONTRAINTES INITIALES
! OUT  VIM    : VARIABLES INTERNES INITIALES
! OUT  VIP    : VARIABLES INTERNES NULLES
! OUT  DEFIMP : =1 SI LES 6 CMP DE EPSI DONT DONNEES
! OUT  COEF   : COEF POUR ADIMENSIONNALISER LE PB
! OUT  INDIMP : TABLEAU D'INDICES =1 SI EPS(I) DONNE
! OUT  FONIMP : FONCTIONS IMPOSEES POUR EPSI OU SIGM
! OUT  CIMPO  : = 1 POUR LA CMP DE EPSI OU SIGM IMPOSEE
! OUT  KEL    : OPERATEUR D'ELASTICITE
! OUT  SDDISC : SD DISCRETISATION
! OUT  PARCRI : PARAMETRES DE CONVERGENCE GLOBAUX
! OUT  PRED   : TYPE DE PREDICTION = 1 SI TANGENTE
! OUT  MATREL : MATRICE TANGENTE = 1 SI ELASTIQUE
! OUT  OPTION : FULL_MECA OU RAPH_MECA
!
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/r8dgrd.h"
#include "asterc/r8vide.h"
#include "asterfort/codent.h"
#include "asterfort/diinst.h"
#include "asterfort/dmat3d.h"
#include "asterfort/eulnau.h"
#include "asterfort/fozero.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/lceqvn.h"
#include "asterfort/nmcrga.h"
#include "asterfort/nmcrli.h"
#include "asterfort/nmcrsu.h"
#include "asterfort/nmdocn.h"
#include "asterfort/r8inir.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/utmess.h"
#include "asterfort/vrcinp.h"
#include "blas/dcopy.h"
#include "blas/dscal.h"
    complex(kind=8) :: cbid
    character(len=24) :: k24bid
!
!
    integer :: ndim, n1, nbvari, nbpar, i, j, k, imate, kpg, ksp, nbocc, n2
    integer :: iepsi, icont, igrad, irota, defimp, indimp(9), ncmp
    integer :: pred, matrel, ic1c2, iforta, imptgt, nbvita, imes(2)
    integer :: iligne, icolon, nbcol, nbvrcm, numins
    character(len=4) :: nomeps(6), nomsig(6), nomgrd(9), optgt
    character(len=8) :: typmod(2), k8b, table, fonimp(9), fongrd(9), f0, vk8(2)
    character(len=8) :: foneps(6), fonsig(6), typpar(*), valef, nomvi(*)
    character(len=16) :: option, nompar(*), predic, matric, fortab
    character(len=19) :: lisins, sddisc, solveu
    character(len=24) :: sderro
    real(kind=8) :: instam, ang(7), sigm(6), epsm(9), vale, rac2
    real(kind=8) :: vim(nbvari), vip(nbvari), vr(*)
    real(kind=8) :: sigi, rep(7), kel(6, 6), cimpo(6, 12)
    real(kind=8) :: angd(3), ang1(1), pgl(3, 3), xyzgau(3), coef, instin
    real(kind=8) :: parcri(*), parcon(9), angeul(3), id(9), dsidep(36)
    real(kind=8) :: sigini(6), epsini(6)
    logical :: lctcd, limpex
!
    data nomeps/'EPXX','EPYY','EPZZ','EPXY','EPXZ','EPYZ'/
    data nomsig/'SIXX','SIYY','SIZZ','SIXY','SIXZ','SIYZ'/
    data nomgrd/'F11','F12','F13','F21','F22','F23','F31','F32','F33'/
    data id/1.d0,0.d0,0.d0, 0.d0,1.d0,0.d0, 0.d0,0.d0,1.d0/
! ----------------------------------------------------------------------
!     INITIALISATIONS
    cbid=(0.d0,0.d0)
    ndim=3
    typmod(1)='3D'
    typmod(2)=' '
    solveu = '&&OP0033'
    rac2=sqrt(2.d0)
!
!     ----------------------------------------
!     RECUPERATION DU NOM DE LA TABLE PRODUITE
!     ----------------------------------------
    call getres(table, k24bid, k24bid)
    iforta=0
    call getvtx(' ', 'FORMAT_TABLE', scal=fortab, nbret=n1)
    if (n1 .ne. 0) then
        if (fortab .eq. 'CMP_LIGNE') then
            iforta=1
        endif
    endif
    nbvita=nbvari
    call getvis(' ', 'NB_VARI_TABLE', scal=k, nbret=n1)
    if (n1 .gt. 0) nbvita=k
    nbvita=min(nbvita,nbvari)
!
    imptgt=0
    call getvtx(' ', 'OPER_TANGENT', scal=optgt, nbret=n1)
    if (n1 .ne. 0) then
        if (optgt .eq. 'OUI') then
            imptgt=1
        endif
    endif
    ncmp=6
    igrad=0
    call getvid(' ', nomgrd(1), scal=fongrd(1), nbret=n1)
    if (n1 .ne. 0) then
        ncmp=9
        igrad=1
    endif
!     SI LE NOMBRE DE VARIABLES INTERNES EST TROP GRAND
!     ON CHANGE DE FORMAT DE TABLE
!     NOMBRE MAXI DE COLONNES DANS UNE TABLE 9999 (CF D4.02.05)
    nbcol=1+ncmp+6+2+nbvita+1+36
    if (nbcol .gt. 9999) then
        iforta=1
    endif
    nompar(1)='INST'
    if (iforta .eq. 0) then
!     LA TABLE CONTIENT L'INSTANT, EPS, SIG, TRACE, VMIS, VARI, NB_ITER
        nbpar=1+ncmp+6+2+nbvita+1
        if (imptgt .eq. 1) nbpar=nbpar+36
        if (igrad .eq. 1) then
            do 132 i = 1, ncmp
                nompar(1+i)=nomgrd(i)
132          continue
        else
            do 131 i = 1, ncmp
                nompar(1+i)=nomeps(i)
131          continue
        endif
        do 13 i = 1, 6
            nompar(1+ncmp+i)=nomsig(i)
13      continue
        nompar(1+ncmp+6+1)='TRACE'
        nompar(1+ncmp+6+2)='VMIS'
        do 11 i = 1, nbvita
            nompar(1+ncmp+6+2+i)(1:1)='V'
            call codent(i, 'G', nompar(1+ncmp+6+2+i)(2:16))
11      continue
        if (imptgt .eq. 1) then
            do 133 i = 1, 6
                do 133 j = 1, 6
                    k=1+ncmp+6+2+nbvari+6*(i-1)+j
                    write(nompar(k),'(A,I1,I1)') 'K',i,j
133              continue
        endif
        nompar(nbpar)='NB_ITER'
        do 10 i = 1, nbpar
            typpar(i)='R'
10      continue
    else
        nbpar=4
        nompar(2)='GRANDEUR'
        nompar(3)='CMP'
        nompar(4)='VALEUR'
        typpar(1)='R'
        typpar(2)='K8'
        typpar(3)='K8'
        typpar(4)='R'
    endif
!
    call tbcrsd(table, 'G')
    call tbajpa(table, nbpar, nompar, typpar)
!
!     ----------------------------------------
!     TRAITEMENT DES ANGLES
!     ----------------------------------------
    call r8inir(7, 0.d0, ang, 1)
    call r8inir(3, 0.d0, angeul, 1)
    call r8inir(3, 0.d0, xyzgau, 1)
    call getvr8('MASSIF', 'ANGL_REP', iocc=1, nbval=3, vect=ang(1),&
                nbret=n1)
    call getvr8('MASSIF', 'ANGL_EULER', iocc=1, nbval=3, vect=angeul,&
                nbret=n2)
!
    if (n1 .gt. 0) then
        ang(1) = ang(1)*r8dgrd()
        if (ndim .eq. 3) then
            ang(2) = ang(2)*r8dgrd()
            ang(3) = ang(3)*r8dgrd()
        endif
        ang(4) = 1.d0
!
!     ECRITURE DES ANGLES D'EULER A LA FIN LE CAS ECHEANT
    else if (n2.gt.0) then
        call eulnau(angeul, angd)
        ang(1) = angd(1)*r8dgrd()
        ang(5) = angeul(1)*r8dgrd()
        if (ndim .eq. 3) then
            ang(2) = angd(2)*r8dgrd()
            ang(3) = angd(3)*r8dgrd()
            ang(6) = angeul(2)*r8dgrd()
            ang(7) = angeul(3)*r8dgrd()
        endif
        ang(4) = 2.d0
    endif
    if (ncmp .eq. 6) then
        call r8inir(9, 0.d0, epsm, 1)
    else
        call dcopy(9, id, 1, epsm, 1)
    endif
    call r8inir(6, 0.d0, sigm, 1)
    call r8inir(nbvari, 0.d0, vim, 1)
    call r8inir(nbvari, 0.d0, vip, 1)
    irota=0
!     ANGLE DE ROTATION
    call getvr8(' ', 'ANGLE', scal=ang1(1), nbret=n1)
    if ((n1.ne.0) .and. (ang1(1).ne.0.d0)) then
!        VERIFS
        irota=1
        call r8inir(9, 0.d0, pgl, 1)
        call dscal(1, r8dgrd(), ang1(1), 1)
        pgl(1,1)=cos(ang1(1))
        pgl(2,2)=cos(ang1(1))
        pgl(1,2)=sin(ang1(1))
        pgl(2,1)=-sin(ang1(1))
        pgl(3,3)=1.d0
! VOIR GENERALISATION A 3 ANGLES AVEC CALL MATROT
    endif
!     ----------------------------------------
!     ETAT INITIAL
!     ----------------------------------------
    call getfac('SIGM_INIT', nbocc)
    if (nbocc .gt. 0) then
        do 15 i = 1, 6
            call getvr8('SIGM_INIT', nomsig(i), iocc=1, scal=sigi, nbret=n1)
            if (n1 .ne. 0) then
                sigm(i)=sigi
            endif
15      continue
        call dscal(3, rac2, sigm(4), 1)
    endif
!
    call getfac('EPSI_INIT', nbocc)
    if (nbocc .gt. 0) then
        do 16 i = 1, 6
            call getvr8('EPSI_INIT', nomeps(i), iocc=1, scal=sigi, nbret=n1)
            if (n1 .ne. 0) then
                epsm(i)=sigi
            endif
16      continue
        call dscal(3, rac2, epsm(4), 1)
    endif
    call getfac('VARI_INIT', nbocc)
    if (nbocc .gt. 0) then
        call getvr8('VARI_INIT', 'VALE', iocc=1, nbval=nbvari, vect=vim,&
                    nbret=n1)
        if (n1 .ne. nbvari) then
            imes(1)=n1
            imes(2)=nbvari
            call utmess('F', 'COMPOR1_72', ni=2, vali=imes)
        endif
    endif
    kpg=1
    ksp=1
    call r8inir(7, 0.d0, rep, 1)
    rep(1)=1.d0
    call dcopy(3, ang, 1, rep(2), 1)
    instam=0.d0
!     ----------------------------------------
!     CHARGEMENT
!     ----------------------------------------
    call r8inir(6*12, 0.d0, cimpo, 1)
    icont=0
    iepsi=0
    igrad=0
    f0='&&CPM_F0'
    call fozero(f0)
    do 23 i = 1, 9
        indimp(i)=0
        fonimp(i)=f0
23  end do
    do 14 i = 1, 6
        call getvid(' ', nomeps(i), scal=foneps(i), nbret=n1)
        call getvid(' ', nomsig(i), scal=fonsig(i), nbret=n2)
        if (n1 .ne. 0) then
            cimpo(i,6+i)=1.d0
            fonimp(i)=foneps(i)
            iepsi=iepsi+1
            indimp(i)=1
        else if (n2.ne.0) then
            cimpo(i,i)=1.d0
            fonimp(i)=fonsig(i)
            icont=icont+1
            indimp(i)=0
        endif
14  end do
    do 141 i = 1, 9
        call getvid(' ', nomgrd(i), scal=fongrd(i), nbret=n1)
        if (n1 .ne. 0) then
            fonimp(i)=fongrd(i)
            igrad=igrad+1
            indimp(i)=2
        endif
141  end do
    defimp=0
    if (iepsi .eq. 6) defimp=1
    if (igrad .eq. 9) defimp=2
    ic1c2=0
!     TRAITEMENT DES RELATIONS LINEAIRES (MOT CLE MATR_C1)
    call getfac('MATR_C1', nbocc)
    if (nbocc .ne. 0) then
        ic1c2=1
        do 55 i = 1, nbocc
            call getvis('MATR_C1', 'NUME_LIGNE', iocc=i, scal=iligne, nbret=n1)
            call getvis('MATR_C1', 'NUME_COLONNE', iocc=i, scal=icolon, nbret=n1)
            call getvr8('MATR_C1', 'VALE', iocc=i, scal=vale, nbret=n1)
            cimpo(iligne,icolon)=vale
55      continue
    endif
    call getfac('MATR_C2', nbocc)
    if (nbocc .ne. 0) then
        ic1c2=1
        do 56 i = 1, nbocc
            call getvis('MATR_C2', 'NUME_LIGNE', iocc=i, scal=iligne, nbret=n1)
            call getvis('MATR_C2', 'NUME_COLONNE', iocc=i, scal=icolon, nbret=n1)
            call getvr8('MATR_C2', 'VALE', iocc=i, scal=vale, nbret=n1)
            cimpo(iligne,icolon+6)=vale
56      continue
    endif
    call getfac('VECT_IMPO', nbocc)
    if (nbocc .ne. 0) then
        do 57 i = 1, nbocc
            call getvis('VECT_IMPO', 'NUME_LIGNE', iocc=i, scal=iligne, nbret=n1)
            call getvid('VECT_IMPO', 'VALE', iocc=i, scal=valef, nbret=n1)
            fonimp(iligne)=valef
57      continue
    endif
    if (ic1c2 .eq. 1) then
        do 58 i = 1, 6
! AFFECTATION DE SIGMA_I=0. SI RIEN N'EST IMPOSE SUR LA LIGNE I
            k=0
            do 59 j = 1, 12
                if (cimpo(i,j) .ne. 0.d0) then
                    k=1
                endif
59          continue
            if (k .eq. 0) then
                cimpo(i,i)=1.d0
            endif
58      continue
        defimp=-1
    endif
!
!     ----------------------------------------
!     ECRITURE ETAT INITIAL DANS TABLE
!     ----------------------------------------
    if (iforta .eq. 0) then
! CONSTRUCTION DES VECTEURS DE DEFORMATION ET CONTRAINTES
! RETIRE LE TERME EN RAC2 SUR COMPOSANTES DE CISAILLEMENT
        call lceqvn(6, epsm, epsini)
        call lceqvn(6, sigm, sigini)
        call dscal(3, 1.d0/rac2, epsini(4), 1)
        call dscal(3, 1.d0/rac2, sigini(4), 1)
! RECOPIE DANS LA TABLE DES VECTEURS SIGINI ET EPSINI
        call dcopy(ncmp, epsini, 1, vr(2), 1)
        call dcopy(6, sigini, 1, vr(ncmp+2), 1)
        vr(1+ncmp+6+1)=0.d0
        vr(1+ncmp+6+2)=0.d0
        call dcopy(nbvita, vim, 1, vr(1+ncmp+6+3), 1)
        vr(1)=instam
!        ajout KTGT
        if (imptgt .eq. 1) then
            call r8inir(36, 0.d0, dsidep, 1)
            call dcopy(36, dsidep, 1, vr(1+6+6+3+nbvari), 1)
        endif
        vr(nbpar)=0
        call tbajli(table, nbpar, nompar, [0], vr,&
                    [cbid], k8b, 0)
    else
        vr(1)=instam
        vk8(1)='EPSI'
        do 551 i = 1, ncmp
            vr(2)=epsm(i)
            vk8(2)=nomeps(i)
            call tbajli(table, nbpar, nompar, [0], vr,&
                        [cbid], vk8, 0)
551      continue
        vk8(1)='SIGM'
        do 552 i = 1, ncmp
            vr(2)=sigm(i)
            vk8(2)=nomsig(i)
            call tbajli(table, nbpar, nompar, [0], vr,&
                        [cbid], vk8, 0)
552      continue
        vk8(1)='VARI'
        do 553 i = 1, nbvita
            vr(2)=vim(i)
            vk8(2)(1:1)='V'
            call codent(i, 'G', vk8(2)(2:8))
            nomvi(i)=vk8(2)
            call tbajli(table, nbpar, nompar, [0], vr,&
                        [cbid], vk8, 0)
553      continue
    endif
!     ----------------------------------------
!     CREATION SD DISCRETISATION
!     ----------------------------------------
    call getvid('INCREMENT', 'LIST_INST', iocc=1, scal=lisins, nbret=n1)
    instin = r8vide()
    call nmcrli(instin, lisins, sddisc)
!     ----------------------------------------
!     CREATION SD ERREUR
!     ----------------------------------------
!
    call nmcrga(sderro)
!
!
!     ----------------------------------------
!     LECTURE DE NEWTON
!     ----------------------------------------
    pred=1
    call getvtx('NEWTON', 'PREDICTION', iocc=1, scal=predic, nbret=n1)
    if (n1 .ne. 0) then
        if (predic .eq. 'ELASTIQUE') pred=0
    endif
    matrel=0
    option='FULL_MECA'
    call getvtx('NEWTON', 'MATRICE', iocc=1, scal=matric, nbret=n1)
    if (n1 .ne. 0) then
        if (matric .eq. 'ELASTIQUE') then
            matrel=1
            pred=0
            option='RAPH_MECA'
        endif
    endif
!     ----------------------------------------
!     LECTURE DES PARAMETRES DE CONVERGENCE
!     ----------------------------------------
    call nmdocn(parcri, parcon)
!     SUBDIVISION AUTOMATIQUE DU PAS DE TEMPS
    limpex = .false.
    lctcd = .false.
    call nmcrsu(sddisc, lisins, parcri, limpex, lctcd,&
                solveu, k24bid)
!     INSTANT INITIAL
    numins=0
    instam = diinst(sddisc, numins)
!     CALCUL DES VARIABLES DE COMMANDE
    call vrcinp(nbvrcm, 2, instam, instam)
!     ----------------------------------------
!     MATRICE ELASTIQUE ET COEF POUR ADIMENSIONNALISER
!     ----------------------------------------
    call dmat3d('PMAT', imate, instam, '+', kpg,&
                ksp, rep, xyzgau, kel)
!     DMAT ECRIT MU POUR LES TERMES DE CISAILLEMENT
    coef=max(kel(1,1),kel(2,2),kel(3,3))
    do 67 j = 4, 6
        kel(j,j) = kel(j,j)*2.d0
        coef=max(coef,kel(j,j))
67  end do
    if (ic1c2 .eq. 1) then
        coef=1.d0
    endif
end subroutine
