subroutine ndnpas(fonact, numedd, numins, sddisc, sddyna,&
                  scotch, valinc, solalg)
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
! TOLE CRP_20
!
    implicit     none
    include 'jeveux.h'
    include 'asterc/r8prem.h'
    include 'asterfort/assert.h'
    include 'asterfort/diinst.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/isfonc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/ndmuap.h'
    include 'asterfort/ndpred.h'
    include 'asterfort/ndynin.h'
    include 'asterfort/ndynkk.h'
    include 'asterfort/ndynlo.h'
    include 'asterfort/ndynre.h'
    include 'asterfort/nmdebg.h'
    include 'blas/dcopy.h'
    integer :: numins
    character(len=24) :: numedd
    character(len=19) :: sddyna, sddisc
    character(len=19) :: solalg(*), valinc(*)
    integer :: fonact(*)
    logical :: scotch
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (DYNAMIQUE)
!
! INITIALISATION DES CHAMPS D'INCONNUES POUR UN NOUVEAU PAS DE TEMPS
!
! ----------------------------------------------------------------------
!
!
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  NUMEDD : NUME_DDL
! IN  NUMINS : NUMERO INSTANT COURANT
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  RESOCO : SD RESOLUTION DU CONTACT
! IN  SDDYNA : SD DYNAMIQUE
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  SCOTCH : .TRUE. SI NOEUD COLLE EN CONTACT CONTINU
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: zero, un, deux
    parameter    (un   = 1.d0,deux = 2.d0)
    parameter    (zero = 0.d0)
!
    integer :: jcfsc, jcfs2
    character(len=24) :: cfsc
    real(kind=8) :: alpha, beta, gamma, theta, phi, unthet, kappa
    real(kind=8) :: instam, instap, deltat
    logical :: lexge, lctcc, lmuap, lgrot, lexpl, lmpas, lhhtc, limpl
    character(len=8) :: k8bid
    logical :: ldepl, lvite, lacce
    logical :: lnewma, ltheta, lkrenk
    real(kind=8) :: coerig, coeamo, coemas
    real(kind=8) :: coeext, coeint, coeequ, coeex2
    integer :: iret, imode
    integer :: neq, nbmodp
    real(kind=8) :: coefd(3), coefv(3), coefa(3)
    real(kind=8) :: coedep, coevit, coeacc
    real(kind=8) :: coerma, coeram, coerri
    real(kind=8) :: coiner
    character(len=19) :: depgem, vitgem, accgem, depgep, vitgep, accgep
    integer :: jdepgm, jvitgm, jaccgm, jdepgp, jvitgp, jaccgp
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> INITIALISATIONS EN DYNAMIQUE'
    endif
!
! --- INITIALISATIONS
!
    call dismoi('F', 'NB_EQUA', numedd, 'NUME_DDL', neq,&
                k8bid, iret)
    instam = diinst(sddisc,numins-1)
    instap = diinst(sddisc,numins)
    deltat = instap - instam
!
! --- FONCTIONNALITES ACTIVEES
!
    lexge = ndynlo(sddyna,'EXPL_GENE')
    lctcc = isfonc(fonact,'CONT_CONTINU')
    lmuap = ndynlo(sddyna,'MULTI_APPUI')
    lgrot = isfonc(fonact,'GD_ROTA')
    lexpl = ndynlo(sddyna,'EXPLICITE')
    lmpas = ndynlo(sddyna,'MULTI_PAS')
    limpl = ndynlo(sddyna,'IMPLICITE')
!
! --- ACCES SD DYNA
!
    cfsc = sddyna(1:15)//'.COEF_SCH'
    call jeveuo(cfsc, 'E', jcfsc)
!
! --- TYPE DE FORMULATION SCHEMA DYNAMIQUE GENERAL
!
    ldepl = ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.1
    lvite = ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.2
    lacce = ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.3
    if (lgrot .and. .not.ldepl) then
        call assert(.false.)
    endif
!
! --- TYPE DE SCHEMA: NEWMARK (ET SES DERIVEES) OU THETA
!
    lnewma = ndynlo(sddyna,'FAMILLE_NEWMARK')
    ltheta = ndynlo(sddyna,'THETA_METHODE')
    lkrenk = ndynlo(sddyna,'KRENK')
    if (.not.(lnewma.or.ltheta.or.lkrenk)) then
        call assert(.false.)
    endif
!
! --- HHT COMPLET (MULTI-PAS)
!
    lhhtc = ndynlo(sddyna,'HHT_COMPLET')
!
! --- COEFFICIENTS DU SCHEMA EN TEMPS
!
    beta = ndynre(sddyna,'BETA')
    gamma = ndynre(sddyna,'GAMMA')
    theta = ndynre(sddyna,'THETA')
    unthet = un-theta
    if (abs(unthet) .le. r8prem()) unthet = un
    phi = ndynre(sddyna,'PHI')
    alpha = ndynre(sddyna,'ALPHA')
    kappa = ndynre(sddyna,'KAPPA')
!
! --- SI NOEUD COLLE, THETA-SCHEMA PUREMENT IMPLICITE
!
    if (lctcc) then
        if (ltheta) then
            if (scotch) then
                theta = 1.d0
            endif
        endif
    endif
!
! --- COEFFICIENTS POUR MATRICES
!
    if (lnewma) then
        if (ldepl) then
            coerig = un
            coeamo = gamma/(beta*deltat)
            coemas = un/(beta*deltat*deltat)
        else if (lacce) then
            coerig = beta*deltat*deltat
            coeamo = gamma*deltat
            coemas = un
        else
            call assert(.false.)
        endif
        if (lhhtc) then
            coeamo = coeamo/(un+alpha)
            coemas = coemas/(un+alpha)
        endif
    else if (ltheta) then
        if (ldepl) then
            coerig = theta
            coeamo = un/(theta*deltat)
            coemas = un/(theta*deltat*deltat)
        else if (lvite) then
            coerig = theta*theta*deltat
            coeamo = theta
            coemas = un/deltat
        else
            call assert(.false.)
        endif
    else if (lkrenk) then
        if (ldepl) then
            coerig = kappa/deux
            coeamo = un/deltat
            coemas = deux/(kappa*deltat*deltat)
        else if (lvite) then
            coerig = (kappa/deux)*(kappa/deux)*deltat
            coeamo = kappa/deux
            coemas = un/deltat
        else
            call assert(.false.)
        endif
    else
        call assert(.false.)
    endif
!
    zr(jcfsc-1+1) = coerig
    zr(jcfsc-1+2) = coeamo
    zr(jcfsc-1+3) = coemas
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... COEF. RIGI.: ',coerig
        write (ifm,*) '<MECANONLINE> ... COEF. AMOR.: ',coeamo
        write (ifm,*) '<MECANONLINE> ... COEF. MASS.: ',coemas
    endif
!
! --- COEFFICIENTS POUR MISE A JOUR DEPL/VITE/ACCE
!
    coedep = 1.d0
    if (lnewma) then
        if (ldepl) then
            coedep = un
            coevit = gamma/(beta*deltat)
            coeacc = un/(beta*deltat*deltat)
        else if (lacce) then
            coedep = beta*deltat*deltat
            coevit = gamma*deltat
            coeacc = un
        else
            call assert(.false.)
        endif
    else if (ltheta) then
        if (ldepl) then
            coedep = un
            coevit = un/(theta*deltat)
            coeacc = deux/(theta*deltat*deltat)
        else if (lvite) then
            coedep = deltat*theta
            coevit = un
            coeacc = deux/deltat
        else
            call assert(.false.)
        endif
    else if (lkrenk) then
        if (ldepl) then
            coedep = un
            coevit = deux/(kappa*deltat)
            coeacc = deux*deux/(kappa*deltat*deltat)
        else if (lvite) then
            coedep = deltat*(kappa/deux)
            coevit = un
            coeacc = deux/deltat
        else
            call assert(.false.)
        endif
    else
        call assert(.false.)
    endif
    zr(jcfsc-1+13) = coedep
    zr(jcfsc-1+14) = coevit
    zr(jcfsc-1+15) = coeacc
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... COEF. DEPL.: ',coedep
        write (ifm,*) '<MECANONLINE> ... COEF. VITE.: ',coevit
        write (ifm,*) '<MECANONLINE> ... COEF. ACCE.: ',coeacc
    endif
!
! --- COEFFICIENTS POUR PREDICTEURS
!
    if (lnewma) then
        if (ldepl) then
            coefd(1) = zero
            coefd(2) = zero
            coefd(3) = zero
            coefv(1) = zero
            coefv(2) = (beta-gamma)/beta
            coefv(3) = ((deux*beta-gamma)*deltat)/(deux*beta)
            coefa(1) = zero
            coefa(2) = -un/(beta*deltat)
            coefa(3) = (deux*beta-un)/(deux*beta)
        else if (lacce) then
            if (lexpl) then
                if (ndynlo(sddyna,'TCHAMWA')) then
                    coefd(1) = un
                    coefd(2) = deltat
                    coefd(3) = deltat*deltat*phi
                    coefv(1) = zero
                    coefv(2) = un
                    coefv(3) = deltat
                    coefa(1) = zero
                    coefa(2) = zero
                    coefa(3) = zero
                else
                    coefd(1) = un
                    coefd(2) = deltat
                    coefd(3) = deltat*deltat/deux
                    coefv(1) = zero
                    coefv(2) = un
                    coefv(3) = deltat*(un-gamma)
                    coefa(1) = zero
                    coefa(2) = zero
                    coefa(3) = zero
                endif
            else
                coefd(1) = un
                coefd(2) = deltat
                coefd(3) = deltat*deltat/deux
                coefv(1) = zero
                coefv(2) = un
                coefv(3) = deltat
                coefa(1) = zero
                coefa(2) = zero
                coefa(3) = un
            endif
        else
            call assert(.false.)
        endif
    else if (ltheta) then
        if (ldepl) then
            coefd(1) = zero
            coefd(2) = zero
            coefd(3) = zero
            coefv(1) = zero
            coefv(2) = (theta-un)/theta
            coefv(3) = zero
            coefa(1) = zero
            coefa(2) = -deux/(theta*deltat)
            coefa(3) = -un
        else if (lvite) then
            coefd(1) = un
            coefd(2) = deltat
            coefd(3) = zero
            coefv(1) = zero
            coefv(2) = un
            coefv(3) = zero
            coefa(1) = zero
            coefa(2) = zero
            coefa(3) = -un
        else
            call assert(.false.)
        endif
    else if (lkrenk) then
        if (ldepl) then
            coefd(1) = un
            coefd(2) = zero
            coefd(3) = zero
            coefv(1) = zero
            coefv(2) = (kappa-deux)/kappa
            coefv(3) = zero
            coefa(1) = zero
            coefa(2) = -deux*deux/(kappa*deltat)
            coefa(3) = -un
        else if (lvite) then
            coefd(1) = un
            coefd(2) = deltat
            coefd(3) = zero
            coefv(1) = zero
            coefv(2) = un
            coefv(3) = zero
            coefa(1) = zero
            coefa(2) = zero
            coefa(3) = -un
        else
            call assert(.false.)
        endif
    else
        call assert(.false.)
    endif
!
    zr(jcfsc-1+4) = coefd(1)
    zr(jcfsc-1+5) = coefd(2)
    zr(jcfsc-1+6) = coefd(3)
    zr(jcfsc-1+7) = coefv(1)
    zr(jcfsc-1+8) = coefv(2)
    zr(jcfsc-1+9) = coefv(3)
    zr(jcfsc-1+10) = coefa(1)
    zr(jcfsc-1+11) = coefa(2)
    zr(jcfsc-1+12) = coefa(3)
!
! --- CALCUL DES PREDICTEURS
!
    call ndpred(sddyna, valinc, solalg)
!
! --- COEFFICIENTS POUR SCHEMAS A PLUSIEURS PAS
! --- COEEXT: COEF. DE PONDERATION DES FORCES EXTERNES
! --- COEINT: COEF. DE PONDERATION DES FORCES INTERNES
! --- COEEQU: COEF. PERMETTANT DE RESPECTER L'EQUILIBRE SU RLES AUTRES
!             TERMES NON PONDERES
!
    if (lmpas) then
        if (lhhtc) then
            coeext = -alpha/(un+alpha)
            coeint = -alpha/(un+alpha)
            coeequ = un/(un+alpha)
            coeex2 = un
        else if (ltheta) then
            coeext = (un-theta)
            if (abs(un-theta) .le. r8prem()) then
                coeext = zero
            endif
            coeint = zero
            coeequ = un
            coeex2 = theta
        else if (lkrenk) then
            if (ldepl) then
                coeext = un/deux
                coeint = zero
                coeequ = un
                coeex2 = un/deux
            else if (lvite) then
                coeext = un/deux
                coeint = zero
                coeequ = un
                coeex2 = un/deux
            endif
        else
            call assert(.false.)
        endif
    else
        coeext = zero
        coeint = zero
        coeequ = un
        coeex2 = un
    endif
    zr(jcfsc-1+16) = coeext
    zr(jcfsc-1+17) = coeequ
    zr(jcfsc-1+18) = coeint
    zr(jcfsc-1+19) = coeex2
!
    if (lmpas) then
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> ... MULTI-PAS F. EXT. N-1: ',&
            coeext
            write (ifm,*) '<MECANONLINE> ... MULTI-PAS F. EXT. N  : ',&
     &                    coeex2
            write (ifm,*) '<MECANONLINE> ... MULTI-PAS F. INT. N-1: ',&
            coeint
            write (ifm,*) '<MECANONLINE> ... MULTI-PAS F. EQU.    : ',&
     &                    coeequ
        endif
    endif
!
! --- COEFFICENT POUR CALCUL FORCE D'INERTIE DE REFERENCE (NDINER)
!
    if (lnewma) then
        if (limpl) then
            coiner = un/(beta*deltat)
        else
            coiner = un/deltat
        endif
    else if (ltheta) then
        if (ldepl) then
            coiner = un/deltat
        else
            coiner = un/deltat
        endif
    else if (lkrenk) then
        if (ldepl) then
            coiner = un/deltat
        else
            coiner = un/deltat
        endif
    else
        coiner = un/deltat
    endif
    zr(jcfsc-1+23) = coiner
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... COEF. FORC. INERTIE REF: ',&
        coiner
    endif
!
! --- COEFFICIENTS DEVANT MATRICE POUR TERME DE RAPPEL DYNAMIQUE
!
    if (ltheta) then
        if (lvite) then
            coerma = zero
            coeram = un
            coerri = theta*deltat
            if (abs(un-theta) .le. r8prem()) then
                coerri = deltat
            endif
        else if (ldepl) then
            if (abs(un-theta) .le. r8prem()) then
                coerma = -un/(theta*deltat)
            else
                coerma = -un/(theta*deltat)
            endif
            coeram = un
            coerri = un
        endif
    else if (lkrenk) then
        if (ldepl) then
            coerma = deux/((deux-kappa)*deltat)
            coeram = un
            coerri = un
        else if (lvite) then
            coerma = zero
            coeram = zero
            coerri = (kappa/deux)*deltat
        endif
    else
        coerma = un
        coeram = un
        coerri = un
    endif
    zr(jcfsc-1+20) = coerma
    zr(jcfsc-1+21) = coeram
    zr(jcfsc-1+22) = coerri
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... COEF. FDYNA RIGI: ',coerri
        write (ifm,*) '<MECANONLINE> ... COEF. FDYNA AMOR: ',coeram
        write (ifm,*) '<MECANONLINE> ... COEF. FDYNA MASS: ',coerma
    endif
!
!   Sauvegarde de INSTAM pour poursuite
!
!      ZR(JCFSC-1+24) = INSTAM
    call jeveuo(cfsc, 'L', jcfs2)
    zr(jcfsc-1+24) = instam
!
! --- INITIALISATION DES CHAMPS D'ENTRAINEMENT EN MULTI-APPUI
!
    if (lmuap) then
        call ndmuap(numins, numedd, sddyna, sddisc)
    endif
!
! --- INITIALISATION DES DEPL. GENERALISES SI PROJECTION MODALE
!
    if (lexge) then
        call ndynkk(sddyna, 'PRMO_DEPGEM', depgem)
        call ndynkk(sddyna, 'PRMO_VITGEM', vitgem)
        call ndynkk(sddyna, 'PRMO_ACCGEM', accgem)
        call ndynkk(sddyna, 'PRMO_DEPGEP', depgep)
        call ndynkk(sddyna, 'PRMO_VITGEP', vitgep)
        call ndynkk(sddyna, 'PRMO_ACCGEP', accgep)
        nbmodp = ndynin(sddyna,'NBRE_MODE_PROJ')
        call jeveuo(accgem, 'E', jaccgm)
        call jeveuo(accgep, 'E', jaccgp)
        call jeveuo(vitgem, 'E', jvitgm)
        call jeveuo(vitgep, 'E', jvitgp)
        call jeveuo(depgem, 'E', jdepgm)
        call jeveuo(depgep, 'E', jdepgp)
        call dcopy(nbmodp, zr(jdepgm), 1, zr(jdepgp), 1)
        call dcopy(nbmodp, zr(jvitgm), 1, zr(jvitgp), 1)
        call dcopy(nbmodp, zr(jaccgm), 1, zr(jaccgp), 1)
!
! --- PREDICTION DEPLACEMENT GENERALISE
!
        do 54 imode = 1, nbmodp
            zr(jdepgp+imode-1) = zr(jdepgm+imode-1) + coefd(2)*zr( jvitgm+imode-1) + coefd(3)*zr(&
                                 &jaccgm+imode-1)
54      continue
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> ...... PRED. DEPL. GENE'
            call nmdebg('VECT', depgep, ifm)
        endif
!
    endif
!
    call jedema()
!
end subroutine
