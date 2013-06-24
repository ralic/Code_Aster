subroutine lc0050(fami, kpg, ksp, ndim, typmod,&
                  imate, compor, crit, instam, instap,&
                  neps, epsm, deps, nsig, sigm,&
                  nvi, vim, option, angmas, nwkin,&
                  wkin, icomp, stress, statev, ndsde,&
                  dsidep, nwkout, wkout, codret)
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
! ======================================================================
!     BUT: INTERFACE POUR ROUTINE D'INTEGRATION LOI DE COMPORTEMENT UMAT
!       IN   FAMI    FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!            KPG,KSP NUMERO DU (SOUS)POINT DE GAUSS
!            NDIM    DIMENSION DE L ESPACE (3D=3,2D=2,1D=1)
!            IMATE    ADRESSE DU MATERIAU CODE
!            COMPOR    COMPORTEMENT DE L ELEMENT
!                COMPOR(1) = RELATION DE COMPORTEMENT (UMAT)
!                COMPOR(2) = NB DE VARIABLES INTERNES
!                COMPOR(3) = TYPE DE DEFORMATION(PETIT,GDEF_LOG)
!            CRIT    CRITERES  LOCAUX, INUTILISES PAR UMAT
!            INSTAM   INSTANT T
!            INSTAP   INSTANT T+DT
!            EPSM   DEFORMATION TOTALE A T EVENTUELLEMENT TOURNEE
!                   DANS LE REPERE COROTATIONNEL SI GDEF_LOG
!            DEPS   INCREMENT DE DEFORMATION EVENTUELLEMENT TOURNEE
!                   DANS LE REPERE COROTATIONNEL SI GDEF_LOG
!            SIGM   CONTRAINTE A T EVENTUELLEMENT TOURNEE...
!            VIM    VARIABLES INTERNES A T + INDICATEUR ETAT T
! ATTENTION : SI MODELE CINEMATIQUE ET GDEF, MODIFIER AUSSI VICIN0.F
!            OPTION     OPTION DE CALCUL A FAIRE
!                          'RIGI_MECA_TANG'> DSIDEP(T)
!                          'FULL_MECA'     > DSIDEP(T+DT) , SIG(T+DT)
!                          'RAPH_MECA'     > SIG(T+DT)
!            ANGMAS  ANGLES DE ROTATION DU REPERE LOCAL, CF. MASSIF
!       OUT  STRESS    CONTRAINTE A T+DT
! !!!!        ATTENTION : ZONE MEMOIRE NON DEFINIE SI RIGI_MECA_TANG
!       OUT  STATEV  VARIABLES INTERNES A T+DT
! !!!!        ATTENTION : ZONE MEMOIRE NON DEFINIE SI RIGI_MECA_TANG
!        IN  WKIN  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES
!            TYPMOD  TYPE DE MODELISATION (3D, AXIS, D_PLAN)
!            ICOMP   NUMERO DU SOUS-PAS DE TEMPS (CF. REDECE.F)
!            NVI     NOMBRE TOTAL DE VARIABLES INTERNES (+9 SI GDEF_HYP)
!       OUT  DSIDEP  MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
!       OUT  CODRET  CODE-RETOUR = 0 SI OK, =1 SINON
! ======================================================================
! aslint: disable=W1504,W0104,W1306
    implicit none
    include 'jeveux.h'
!
    include 'asterc/r8nnem.h'
    include 'asterc/umatwp.h'
    include 'asterfort/assert.h'
    include 'asterfort/infniv.h'
    include 'asterfort/lceqvn.h'
    include 'asterfort/lcicma.h'
    include 'asterfort/matrot.h'
    include 'asterfort/pmat.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/rcvarc.h'
    include 'asterfort/tecael.h'
    include 'asterfort/verift.h'
    include 'blas/daxpy.h'
    include 'blas/dcopy.h'
    include 'blas/dscal.h'
    integer :: imate, ndim, kpg, ksp, codret, icomp, nvi, iret, nprop2
    integer :: nprops, ntens, ndi, nshr, i, nstatv, npt, noel, layer, npred
    integer :: kspt, kstep, kinc, idbg, j, ifm, niv, nwkin, nwkout, iret2
    parameter     ( nprops = 197)
    parameter     ( npred = 6)
    integer :: codrel(nprops+3), neps, nsig, ndsde, iadzi, iazk24
    real(kind=8) :: crit(*), angmas(*), tm, tp, tref, propl(nprops+3)
    real(kind=8) :: instam, instap, drot(3, 3), dstran(9), props(nprops+3)
    real(kind=8) :: epsm(6), deps(6), wkin(nwkin), wkout(nwkout), epsthm
    real(kind=8) :: sigm(6), stress(6), depsth(6), sse, spd, scd, time(2)
    real(kind=8) :: vim(*), statev(nvi), bendom, kdessm, bendop, kdessp
    real(kind=8) :: predef(npred), dpred(npred), vrcm, vrcp, valreb(2)
    real(kind=8) :: hydrm, hydrp, sechm, sechp, sref, epsbp, epsbm, epsthp
    real(kind=8) :: ddsdde(36), dfgrd0(3, 3), dfgrd1(3, 3)
    real(kind=8) :: ddsddt(6), drplde(6), celent, stran(9), dsidep(6, 6)
    real(kind=8) :: dtime, temp, dtemp, coords(3), rpl, pnewdt, drpldt
    real(kind=8) :: depst1, epsth1, epsth(6), rac2, usrac2, drott(3, 3)
    character(len=16) :: compor(*), option
    character(len=8) :: typmod(*), nomres(nprops), nomreb(2), lvarc(npred)
    character(len=*) :: fami
    character(len=80) :: cmname
    common/tdim/  ntens  , ndi
!     POUR TECAEL
    character(len=128) :: nomlib
    character(len=16) :: nomsub
    integer :: ii, dimaki, nbcoef, icodrb(2)
!     DIMAKI = DIMENSION MAX DE LA LISTE DES RELATIONS KIT
    parameter (dimaki=9)
    data idbg/1/
    data nomres/'C1','C2','C3','C4','C5','C6','C7','C8','C9','C10',&
     &'C11','C12','C13','C14','C15','C16','C17','C18','C19','C20','C21',&
     &'C22','C23','C24','C25','C26','C27','C28','C29','C30','C31','C32',&
     &'C33','C34','C35','C36','C37','C38','C39','C40','C41','C42','C43',&
     &'C44','C45','C46','C47','C48','C49','C50','C51','C52','C53','C54',&
     &'C55','C56','C57','C58','C59','C60','C61','C62','C63','C64','C65',&
     &'C66','C67','C68','C69','C70','C71','C72','C73','C74','C75','C76',&
     &'C77','C78','C79','C80','C81','C82','C83','C84','C85','C86','C87',&
     &'C88','C89','C90','C91','C92','C93','C94','C95','C96','C97','C98',&
     &'C99','C100','C101','C102','C103','C104','C105','C106','C107',&
     &'C108','C109','C110','C111','C112','C113','C114','C115','C116',&
     &'C117','C118','C119','C120','C121','C122','C123','C124','C125',&
     &'C126','C127','C128','C129','C130','C131','C132','C133','C134',&
     &'C135','C136','C137','C138','C139','C140','C141','C142','C143',&
     &'C144','C145','C146','C147','C148','C149','C150','C151','C152',&
     &'C153','C154','C155','C156','C157','C158','C159','C160','C161',&
     &'C162','C163','C164','C165','C166','C167','C168','C169','C170',&
     &'C171','C172','C173','C174','C175','C176','C177','C178','C179',&
     &'C180','C181','C182','C183','C184','C185','C186','C187','C188',&
     &'C189','C190','C191','C192','C193','C194','C195','C196','C197'/
    data lvarc/'SECH','HYDR','IRRA','NEUT1','NEUT2','CORR'/
!
!     NTENS  :  NB TOTAL DE COMPOSANTES TENSEURS
!     NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
! ======================================================================
!
!     NUMERO D'ELEMENT SEULEMENT SI ON N'EST PAS DANS CALC_POINT_MAT
    noel=0
    if (compor(1)(9:14) .ne. 'OP0033') then
!        NUMERO D'ELEMENT
        call tecael(iadzi, iazk24)
        noel=zi(iadzi)
    endif
!
    ntens=2*ndim
    ndi=3
    nshr=ntens-ndi
    codret=0
    rac2=sqrt(2.d0)
    usrac2=rac2*0.5d0
!
!     IMPRESSIONS EVENTUELLES EN DEBUG
    call infniv(ifm, niv)
!
!     PARAMETRES UMAT STOCKES DANS 'KIT1-KIT9'
    do 10 ii = 1, dimaki-1
        nomlib(16*(ii-1)+1:16*ii) = compor(7+ii)
10  continue
    nomsub = compor(7+dimaki)
!
!     LECTURE DES PROPRIETES MATERIAU (MOT-CLE UMAT DE DEFI_MATERIAU)
    call r8inir(nprops, r8nnem(), props, 1)
!
!     LECTURE DU PREMIER PARAMETRE NB, FACULTATIF
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'UMAT', 0, ' ', 0.d0,&
                1, 'NB_VALE', propl(1), codrel, 0)
    if (codrel(1) .eq. 0) then
        nbcoef=nint(propl(1))
    else
        nbcoef=nprops
    endif
!     lecture des autres parametres
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'UMAT', 0, ' ', 0.d0,&
                nbcoef, nomres, propl, codrel, 0)
!     COMPTAGE DU NOMBRE DE PROPRIETES
!     CODREL(I)=0 SI LE PARAMETRE EXISTE, 1 SINON
    nprop2=0
    if ((niv.ge.2) .and. (idbg.eq.1)) then
        write(ifm,*)' '
        write(ifm,*)'COEFFICIENTS MATERIAU'
    endif
    do 20 i = 1, nbcoef
        if (codrel(i) .eq. 0) then
            nprop2=nprop2+1
            props(nprop2)=propl(i)
            if ((niv.ge.2) .and. (idbg.eq.1)) then
                write(ifm,*) nomres(i),props(nprop2)
            endif
        endif
20  continue
!
! APPEL DE RCVARC POUR LE CALCUL DE LA TEMPERATURE
! RAISON: PASSAGE A UMAT DE LA TEMPERATURE
    call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                ksp, tm, iret)
    if (iret .ne. 0) tm=0.d0
    call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                ksp, tp, iret)
    if (iret .ne. 0) tp=0.d0
    call rcvarc(' ', 'TEMP', 'REF', fami, kpg,&
                ksp, tref, iret)
    if (iret .ne. 0) tref=0.d0
!     CALCUL DES DEFORMATIONS DE DILATATION THERMIQUE
    call verift(fami, kpg, ksp, 'T', imate,&
                'ELAS', 1, depst1, iret)
    if (iret .ne. 0) depst1=0.d0
    call verift(fami, kpg, ksp, '-', imate,&
                'ELAS', 1, epsth1, iret)
    if (iret .ne. 0) epsth1=0.d0
!
! APPEL DE RCVARC POUR EXTRAIRE TOUTES LES VARIABLES DE COMMANDE
    call r8inir(npred, r8nnem(), predef, 1)
    call r8inir(npred, r8nnem(), dpred, 1)
!     SECHAGE
    call rcvarc(' ', lvarc(1), '-', fami, kpg,&
                ksp, vrcm, iret)
    if (iret .eq. 0) then
        predef(1)=vrcm
        call rcvarc('F', lvarc(1), '+', fami, kpg,&
                    ksp, vrcp, iret2)
        dpred(1)=vrcp-vrcm
!        RETRAIT DESSICATION
        nomreb(1)='K_DESSIC'
        call rcvalb(fami, kpg, ksp, '-', imate,&
                    ' ', 'ELAS', 0, ' ', 0.d0,&
                    1, nomreb, valreb, icodrb, 1)
        kdessm = valreb(1)
        call rcvalb(fami, kpg, ksp, '+', imate,&
                    ' ', 'ELAS', 0, ' ', 0.d0,&
                    1, nomreb, valreb, icodrb, 1)
        kdessp = valreb(1)
        call rcvarc(' ', 'SECH', 'REF', fami, kpg,&
                    ksp, sref, iret2)
        if (iret2 .ne. 0) sref=0.d0
        sechm=predef(1)
        sechp=predef(1)+dpred(1)
        epsbm=-kdessm*(sref-sechm)
        epsbp=-kdessp*(sref-sechp)
        epsthm=epsth1+epsbm
        epsthp=epsth1+depst1+epsbp
        depst1=epsthp-epsthm
        epsth1=epsthm
    endif
!     HYDRATATION
    call rcvarc(' ', lvarc(2), '-', fami, kpg,&
                ksp, vrcm, iret)
    if (iret .eq. 0) then
        predef(2)=vrcm
        call rcvarc('F', lvarc(2), '+', fami, kpg,&
                    ksp, vrcp, iret2)
        dpred(2)=vrcp-vrcm
!        RETRAIT ENDOGENE
        nomreb(1)='B_ENDOGE'
        call rcvalb(fami, kpg, ksp, '-', imate,&
                    ' ', 'ELAS', 0, ' ', 0.d0,&
                    1, nomreb, valreb, icodrb, 1)
        bendom = valreb(1)
        call rcvalb(fami, kpg, ksp, '+', imate,&
                    ' ', 'ELAS', 0, ' ', 0.d0,&
                    1, nomreb, valreb, icodrb, 1)
        bendop = valreb(1)
        hydrm=predef(2)
        hydrp=predef(2)+dpred(2)
        epsbm=-bendom*hydrm
        epsbp=-bendop*hydrp
        epsthm=epsth1+epsbm
        epsthp=epsth1+depst1+epsbp
        depst1=epsthp-epsthm
        epsth1=epsthm
    endif
    do 30 i = 3, npred
        call rcvarc(' ', lvarc(i), '-', fami, kpg,&
                    ksp, vrcm, iret)
        if (iret .eq. 0) then
            predef(i)=vrcm
            call rcvarc('F', lvarc(i), '+', fami, kpg,&
                        ksp, vrcp, iret2)
            dpred(i)=vrcp-vrcm
        endif
30  continue
!
!
! CAS DES GRANDES DEFORMATIONS : ON VEUT F- ET F+
!
    if (neps .eq. 9) then
!
        call dcopy(neps, epsm, 1, dfgrd0, 1)
        if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
            call pmat(3, deps, dfgrd0, dfgrd1)
        else
            call dcopy(neps, dfgrd0, 1, dfgrd1, 1)
        endif
        call r8inir(neps, 0.d0, stran, 1)
        call r8inir(neps, 0.d0, dstran, 1)
!
    else if (neps.eq.6) then
!
! PETITES DEFORMATIONS : DEFORMATION - DEFORMATION THERMIQUE
        call r8inir(neps, 0.d0, depsth, 1)
        call r8inir(ndi, depst1, depsth, 1)
        if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
            call dcopy(neps, deps, 1, dstran, 1)
            call daxpy(neps, -1.d0, depsth, 1, dstran,&
                       1)
! TRAITEMENT DES COMPOSANTES 4,5,6 : DANS UMAT, GAMMAXY,XZ,YZ
            call dscal(3, rac2, dstran(4), 1)
        else
            call r8inir(neps, 0.d0, dstran, 1)
        endif
!
        call r8inir(neps, 0.d0, epsth, 1)
        call r8inir(ndi, epsth1, epsth, 1)
        call dcopy(neps, epsm, 1, stran, 1)
        call daxpy(neps, -1.d0, epsth, 1, stran,&
                   1)
        call dscal(3, rac2, stran(4), 1)
!
        call r8inir(9, 0.d0, dfgrd0, 1)
        call r8inir(9, 0.d0, dfgrd1, 1)
    else
        call assert(.false.)
    endif
!
    if (compor(3) .eq. 'GDEF_LOG') then
        nstatv=nvi-6
    else
        nstatv=nvi
    endif
!
    time(1)=instap-instam
    time(2)=instam
    dtime=instap-instam
    temp=tm
    dtemp=tp-tm
    cmname=compor(1)
!
    call r8inir(3, r8nnem(), coords, 1)
    call matrot(angmas, drott)
!
    do 100,i = 1,3
    do 90,j = 1,3
    drot(j,i) = drott(i,j)
90  continue
100 continue
!
    celent=wkin(1)
    npt=kpg
    layer=1
    kspt=ksp
    kstep=icomp
    kinc=1
!     initialisations des arguments inutilises
    sse=0.d0
    spd=0.d0
    scd=0.d0
    rpl=0.d0
    call r8inir(6, 0.d0, ddsddt, 1)
    call r8inir(6, 0.d0, drplde, 1)
    drpldt=0.d0
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        if ((niv.ge.2) .and. (idbg.eq.1)) then
            write(ifm,*)' '
            write(ifm,*)'AVANT APPEL UMAT, INSTANT=',time(2)+dtime
            write(ifm,*)'     NOM LIBRAIRIE : '//nomlib
            write(ifm,*)'       NOM ROUTINE : '//nomsub
            write(ifm,*)'NUMERO ELEMENT=',noel
            write(ifm,*)'DEFORMATIONS INSTANT PRECEDENT STRAN='
            write(ifm,'(6(1X,E11.4))') (stran(i),i=1,ntens)
            write(ifm,*)'ACCROISSEMENT DE DEFORMATIONS DSTRAN='
            write(ifm,'(6(1X,E11.4))') (dstran(i),i=1,ntens)
            write(ifm,*)'CONTRAINTES INSTANT PRECEDENT STRESS='
            write(ifm,'(6(1X,E11.4))') (sigm(i),i=1,ntens)
            write(ifm,*)'NVI=',nstatv,' VARIABLES INTERNES STATEV='
            write(ifm,'(10(1X,E11.4))') (vim(i),i=1,nstatv)
            write(ifm,*)'TEMPERATURE ET INCREMENT'
            write(ifm,'(2(1X,E11.4))') temp,dtemp
            write(ifm,*) 'VARIABLES DE COMMANDE ET INCREMENTS'
            do 70 i = 1, npred
                write(ifm,'(A8,2(1X,E11.4))') lvarc(i),predef(i),&
                dpred(i)
70          continue
        endif
    endif
!
    pnewdt=1.d0

!   pour MFRONT ddsdde(1)= type de matrice tangente
!   ddsdde(1) <0 : matrice de prédiction
!   -1, matrice elastique initiale (sans endommagement) 
!   -2, matrice secante (avec endommagement)
!   -3, matrice tangente.
!   ddsdde(1) >0 : matrice tangente (FULL_MECA, FULL_MECA_ELAS)
!    1 matrice elastique initiale (sans endommagement)
!    2 matrice secante (avec endommagement)
!    3 matrice tangente
!    4 matrice tangente cohérente
    
    ddsdde=1.d0
    if (option .eq. 'RIGI_MECA_TANG') then
        ddsdde(1)=-3.d0
    else if (option .eq. 'RIGI_MECA_ELAS' ) then
        ddsdde(1)=-2.d0
    else if (option .eq. 'FULL_MECA_ELAS' ) then
        ddsdde(1)= 2.d0
    else if (option .eq. 'FULL_MECA' ) then
        ddsdde(1)= 4.d0
    else if (option .eq. 'RAPH_MECA' ) then
        ddsdde(1)= 0.d0
    endif

    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
!
        call dcopy(nsig, sigm, 1, stress, 1)
        call dscal(3, usrac2, stress(4), 1)
!
        call lceqvn(nstatv, vim, statev)
!
        call umatwp(nomlib, nomsub, stress, statev, ddsdde,&
                    sse, spd, scd, rpl, ddsddt,&
                    drplde, drpldt, stran, dstran, time,&
                    dtime, temp, dtemp, predef, dpred,&
                    cmname, ndi, nshr, ntens, nstatv,&
                    props, nprop2, coords, drot, pnewdt,&
                    celent, dfgrd0, dfgrd1, noel, npt,&
                    layer, kspt, kstep, kinc)
!
    else if (option(1:9).eq. 'RIGI_MECA') then
        call r8inir(6, 0.d0, dstran, 1)
        call umatwp(nomlib, nomsub, sigm, vim, ddsdde,&
                    sse, spd, scd, rpl, ddsddt,&
                    drplde, drpldt, stran, dstran, time,&
                    dtime, temp, dtemp, predef, dpred,&
                    cmname, ndi, nshr, ntens, nstatv,&
                    props, nprop2, coords, drot, pnewdt,&
                    celent, dfgrd0, dfgrd1, noel, npt,&
                    layer, kspt, kstep, kinc)
    endif
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        if ((niv.ge.2) .and. (idbg.eq.1)) then
            write(ifm,*)' '
            write(ifm,*)'APRES APPEL UMAT, STRESS='
            write(ifm,'(6(1X,E11.4))') (stress(i),i=1,ntens)
            write(ifm,*)'APRES APPEL UMAT, STATEV='
            write(ifm,'(10(1X,E11.4))')(statev(i),i=1,nstatv)
        endif
    endif
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        call dscal(3, rac2, stress(4), 1)
    endif
!
    if (option(1:9) .eq. 'RIGI_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        call r8inir(36, 0.d0, dsidep, 1)
        call lcicma(ddsdde, ntens, ntens, ntens, ntens,&
                    1, 1, dsidep, 6, 6,&
                    1, 1)
        do 40 i = 1, 6
            do 40 j = 4, 6
                dsidep(i,j) = dsidep(i,j)*rac2
40          continue
        do 50 i = 4, 6
            do 50 j = 1, 6
                dsidep(i,j) = dsidep(i,j)*rac2
50          continue
        if ((niv.ge.2) .and. (idbg.eq.1)) then
            write(ifm,*)'APRES APPEL UMAT,OPERATEUR TANGENT DSIDEP='
            do 60 i = 1, 6
                write(ifm,'(6(1X,E11.4))') (dsidep(i,j),j=1,6)
60          continue
        endif
    endif
!
    if (pnewdt .lt. 0.99d0) codret=1
    idbg=0
!
end subroutine
