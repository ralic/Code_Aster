subroutine dtmprep(sd_dtm_)
    implicit none
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: hassan.berro at edf.fr
!
! dtmprep : Preparation of a DYNA_VIBRA//TRAN/GENE calculation, by reading
!           and saving the all information regarding the transient calculation
!           to be undertaken.
!
#include "jeveux.h"
#include "blas/dcopy.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterc/r8prem.h"
#include "asterfort/ajlagr.h"
#include "asterfort/assert.h"
#include "asterfort/copmat.h"
#include "asterfort/copmod.h"
#include "asterfort/cresol.h"
#include "asterfort/dismoi.h"
#include "asterfort/dtmallo.h"
#include "asterfort/dtmget.h"
#include "asterfort/dtminivec.h"
#include "asterfort/dtmprep_arch.h"
#include "asterfort/dtmprep_damp.h"
#include "asterfort/dtmprep_fsi.h"
#include "asterfort/dtmprep_noli.h"
#include "asterfort/dtmsav.h"
#include "asterfort/extdia.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/inicou.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdptem.h"
#include "asterfort/mdrecf.h"
#include "asterfort/mdrede.h"
#include "asterfort/mdrevi.h"
#include "asterfort/mdveri.h"
#include "asterfort/mtdscr.h"
#include "asterfort/mgutdm.h"
#include "asterfort/r8inir.h"
#include "asterfort/trlds.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
!   -0.1- Input/output arguments
    character(len=*)          , intent(in) :: sd_dtm_
!
!   -0.2- Local variables
    integer          :: iret, append, substruc, vali(1), nbmode
    integer          :: nbstor, jmas1, jmas2, jrefe, jrig1
    integer          :: jrig2, lamor, jamo1, jamo2, i
    integer          :: neq, jbase, nexcit, nexcir
    integer          :: jvecr, ntotex, jcoefm, jiadve, jinumo
    integer          :: jidesc, jnomfo, jnodep, jnovit, jnoacc
    integer          :: jpsdel, jgyog, jrgyg, ng2, jscdeg
    integer          :: nbrede, jrede, jfond, nbrevi, jrevi
    integer          :: jfonv, descr, descm, desca, lagrcorr
    integer          :: jrefa, nbnli, nbpas, nbpal
    integer          :: ibid, nbbas, nbmodi, piv
    integer          :: nbsst, j, jmas3, nbmody
    real(kind=8)     :: acrit, agene, valr(2), omeg2
    real(kind=8)     :: vrotat, dt, dtmax, dtmin, tinit
    real(kind=8)     :: tfin, dtedyo, epsi
    character(len=8) :: sd_dtm, nomres, tran, calcres, masgen
    character(len=8) :: riggen, amogen, basemo, matass
    character(len=8) :: resgen, basemo2, monmot, foncv, fonca
    character(len=8) :: gyogen, rgygen, k8var, modgen
    character(len=8) :: mastem, amotem
    character(len=14):: numddl, numgeg
    character(len=16):: typrep, typbas, typco, method, methods(_DTM_NB_SCHEMAS)
    character(len=19):: nomstg
    character(len=24):: typres, nomcmd, nume24, nomstr, valk(1)
    character(len=24):: solver, lisins
!
    integer, pointer           :: desc(:)  => null()
    integer, pointer           :: scde(:)  => null()
    integer, pointer           :: nequ(:)  => null()
    real(kind=8), pointer      :: chodep(:)=> null()
    real(kind=8), pointer      :: chopar(:)=> null()
    real(kind=8), pointer      :: conl(:)  => null()
    real(kind=8), pointer      :: puls(:)  => null()
    real(kind=8), pointer      :: puls2(:) => null()
    character(len=24), pointer :: refa(:)  => null()
    character(len=24), pointer :: refag(:) => null()
    character(len=8) , pointer :: chonoe(:)=> null()

    data  methods /'DIFF_CENTRE     ', 'DEVOGE          ', 'NEWMARK         ', &
                   'RUNGE_KUTTA_32  ', 'RUNGE_KUTTA_54  ', 'ADAPT_ORDRE1    ', &
                   'ADAPT_ORDRE2    ', 'ITMI            '/

!
!   0 - Initializations
    call jemarq()
    call infmaj()
    sd_dtm = sd_dtm_
    epsi   = r8prem()
!
!   --------------------------------------------------------------------------------------
!   1 - Start with verifications of the command syntax which are not possible 
!       within the catalogue
!   --------------------------------------------------------------------------------------
    call mdveri()
!
!   --------------------------------------------------------------------------------------
!   2 - Determine the integration method, whether a new calculation is requested or 
!       a continue of a preceding one, and initialize a linear solver
!   --------------------------------------------------------------------------------------
!
    call getres(nomres, typres, nomcmd)
    call getvtx('SCHEMA_TEMPS', 'SCHEMA', iocc=1, scal=method)
    calcres = nomres
    append = 0
    call getvid('ETAT_INIT', 'RESULTAT', iocc=1, scal=tran, nbret=iret)
    if (iret .ne. 0) then
!        --- Identical output result name to that considered as initial state
!            The integration results will have to be saved in a temporary location
!            and then appended at the end of the command.
        if (tran .eq. nomres) then 
            calcres = '&&OP0029'
            append = 1
        endif
    endif
    call dtmget(sd_dtm, _SOLVER, savejv=solver)
    call cresol(solver)
!    
    call dtmsav(sd_dtm, _RESU_SD ,1, kscal=nomres)
    call dtmsav(sd_dtm, _CALC_SD ,1, kscal=calcres)
    call dtmsav(sd_dtm, _APPND_SD,1, iscal=append)

    do i = 1, _DTM_NB_SCHEMAS
        if (method.eq.methods(i)) goto 5
    end do
5   continue
    if (i.gt._DTM_NB_SCHEMAS) then
        ASSERT(.false.)
    end if
    call dtmsav(sd_dtm, _SCHEMA_I  ,1, iscal=i)
    call dtmsav(sd_dtm, _SCHEMA  ,1, kscal=method)

!
!   --------------------------------------------------------------------------------------
!   3 - Information on the projection (modal) basis, frequencies, and eigen vectors
!   --------------------------------------------------------------------------------------
!
!   --- 4.1 - Modal basis and number of modes
    call getvid(' ', 'MATR_MASS', scal=masgen, nbret=iret)

    if (iret.eq.0) then
        ! write(*,*) "No mass matrix has been found as an input"
        ASSERT(.false.)
    end if
    call jeveuo(masgen//'           .REFA', 'L', vk24=refa)
    basemo = refa(1)(1:8)
    call jeveuo(masgen//'           .DESC', 'L', vi=desc)
    nbmode = desc(2)
    typbas = 'MODE_MECA'

!   --- Base type : MODE_MECA <=> the projected matrices (K,M) are both diagonal
!   ---           : BASE_MODA <=> at least one of the projected matrices is not diagonal
!
!   Note : this property is determined in section 4 after checking the matrices K and M

    call gettco(basemo,typco)
    if (typco(1:9) .eq. 'MODE_MECA') then
        call dismoi('NUME_DDL', basemo, 'RESU_DYNA', repk=numddl)
        call dismoi('NB_EQUA', numddl, 'NUME_DDL', repi=neq)
    else if (typco(1:9).eq.'MODE_GENE') then
        call dismoi('REF_RIGI_PREM', basemo, 'RESU_DYNA', repk=matass)
        call dismoi('NOM_NUME_DDL', matass, 'MATR_ASSE', repk=numddl)
        call jeveuo(numddl(1:14)//'.NUME.NEQU', 'L', vi=nequ)
        neq = nequ(1)
    else 
!       --- Substructuring case, basis vectors can not be retrieved
        goto 10
    endif
!
    call dtminivec(sd_dtm, _BASE_VEC, nbmode*neq, address=jbase)
    call copmod(basemo, numer=numddl, bmodr=zr(jbase), nbmodes=nbmode, nequa=neq)
    call dtmsav(sd_dtm, _NB_PHYEQ,1, iscal=neq)
!   Physical DOF numbering, note that in substructuring cases the nume_ddl_gene is saved
!   under this parameter (NUM_DDL) of the sd_dtm (section 4.0)
    call dtmsav(sd_dtm, _NUM_DDL,1, kscal=numddl)

10  continue
!
!   --------------------------------------------------------------------------------------
!   4 - Global information on the matrices from the command : K / M / C
!   --------------------------------------------------------------------------------------
!
!   --- 4.0 - Calculation type : simple projection or sub-structuring
    nume24(1:14) = refa(2)(1:14)
    call jeveuo(nume24(1:14)//'.NUME.REFN', 'L', jrefe)
    call gettco(zk24(jrefe), typrep)
    substruc = 0
    if (typrep(1:11).eq.'MODELE_GENE') then 
        substruc = 1
        modgen = zk24(jrefe)(1:8)
        call jelira(modgen//'      .MODG.SSNO', 'NOMMAX', nbsst)
        call dtmsav(sd_dtm, _NUM_DDL,1, kscal=nume24(1:14))
    endif
    call dtmsav(sd_dtm, _SUB_STRU,1, iscal=substruc)
    lagrcorr = 0

!   --- 4.1 - Extraction of the mass M-matrix, full and diagonal
    nomstr = nume24(1:14)//'.SLCS'
    call jeveuo(nomstr(1:19)//'.SCDE', 'L', vi=scde)
    nbstor = scde(1)*scde(4)
!   --- Diagonal terms extraction and saving
    call dtminivec(sd_dtm, _MASS_DIA, nbmode, address=jmas1)
    call extdia(masgen, nume24, substruc, zr(jmas1))
    if (nbstor.gt.nbmode) then
!       --- Full matrix extraction and saving
        call dtminivec(sd_dtm, _MASS_FUL, nbmode*nbmode, address=jmas2)
        call copmat(masgen, zr(jmas2))

!       --- Factorized matrix saving
        call dtminivec(sd_dtm, _MASS_FAC, nbmode*nbmode,address=jmas3)
        call dcopy(nbmode*nbmode, zr(jmas2),1,zr(jmas3),1)
        call trlds(zr(jmas3), nbmode, nbmode, iret)

!       --- Zero pivot : correction is required (case of sub-structuring)
        if (iret.ne.0) then
            lagrcorr = 1
            mastem = '&&29&MAS'
            call getvid(' ', 'MATR_RIGI', scal=riggen)
            call ajlagr(riggen, masgen, mastem)
            call copmat(mastem, zr(jmas2))
            call dcopy(nbmode*nbmode, zr(jmas2),1,zr(jmas3),1)
            call trlds(zr(jmas3), nbmode, nbmode, piv)
            if (piv.ne.0) then
                write(*,*) 'Substructuring case, zero pivot on mass matrix line ', piv
                ASSERT(.false.)
            end if
        end if
        typbas = 'BASE_MODA'
    endif
    call dtmsav(sd_dtm, _MASS_MAT,1, kscal=masgen)
    call dtmsav(sd_dtm, _BASE_MOD,1, kscal=basemo)
    call dtmsav(sd_dtm, _NB_MODES,1, iscal=nbmode)   
!
!
!   -------------------------------------------------------------------------------------
!
!   --- 4.2 - Extraction of the stiffness K-matrix, full and diagonal
    call getvid(' ', 'MATR_RIGI', scal=riggen)
    call jeveuo(riggen//'           .REFA', 'L', vk24=refa)
    nume24(1:14) = refa(2)(1:14)
    nomstr = nume24(1:14)//'.SLCS'
    call jeveuo(nomstr(1:19)//'.SCDE', 'L', vi=scde)
    nbstor = scde(1)*scde(4)
!   --- Diagonal terms extraction and saving
    call dtminivec(sd_dtm, _RIGI_DIA, nbmode, address=jrig1)
    call extdia(riggen, nume24, substruc, zr(jrig1))
    if (nbstor.gt.nbmode) then
!       --- Full matrix extraction and saving
        call dtminivec(sd_dtm, _RIGI_FUL, nbmode*nbmode, address=jrig2)
        call copmat(riggen, zr(jrig2))
        typbas = 'BASE_MODA'
    endif
    call dtmsav(sd_dtm, _RIGI_MAT,1, kscal=riggen)
    call dtmsav(sd_dtm, _TYP_BASE,1, kscal=typbas)

!   --- Calculation of the frequencies associated to the modes from the diagonal
!       mass and stiffness terms
    AS_ALLOCATE(vr=puls , size=nbmode)
    AS_ALLOCATE(vr=puls2, size=nbmode)
    do i = 1, nbmode
        puls (i) = 0.d0
        puls2(i) = 0.d0
    end do
    if (substruc.eq.0) then
!       --- Direct calculation case, all modes can be supposed dynamic and some
!           frequency can hereafter be associated to each generalized dof
        do i = 1, nbmode
            omeg2 = abs(zr(jrig1+i-1)/zr(jmas1+i-1))
            puls (i) = sqrt(omeg2)
            puls2(i) = omeg2
        enddo
        nbmody = nbmode
    else
!       --- Substructuring case, loop over each substructure, and then associate a
!           frequency for the dynamic modes
        nbmodi = 0
        nbmody = 0
        do i = 1, nbsst
            call mgutdm(modgen, ' ', i, 'NOM_BASE_MODALE', ibid, basemo)
            call dismoi('NB_MODES_DYN', basemo, 'RESULTAT', repi=nbbas)
            do j = 1, nbbas
                omeg2 = abs(zr(jrig1+nbmodi+j-1)/zr(jmas1+nbmodi+j-1))
                puls (nbmodi+j) = sqrt(omeg2)
                puls2(nbmodi+j) = omeg2
            end do
            nbmody = nbmody + nbbas
            call dismoi('NB_MODES_TOT', basemo, 'RESULTAT', repi=nbbas)
            nbmodi = nbmodi + nbbas
        end do
!       --- Substructuing : saved is the modal basis corresponding to the last 
!           dynamic substructure (??? Special care should be taken thereafter ???)
!           NB_MOD_D represents the number of dynamic modes across all substructures
        call dtmsav(sd_dtm, _BASE_MOD,1,kscal=basemo)
        call dtmsav(sd_dtm, _NB_MOD_D,1,iscal=nbmody)
    endif
    call dtmsav(sd_dtm, _OMEGA   ,nbmody,rvect=puls)
    call dtmsav(sd_dtm, _OMEGA_SQ,nbmody,rvect=puls2)
    AS_DEALLOCATE(vr=puls2)
!   Note : puls is kept in memory for later usage with mdptem and mdchoc
!
!   -------------------------------------------------------------------------------------
!
!   --- 4.3 - Extraction of the damping C-matrix, full and diagonal
    lamor = 0
!   --- lamor signifies that damping is read from a list : AMOR_REDUIT / LIST_AMOR and 
!       no damping matrix is used
    call getvid(' ', 'MATR_AMOR', scal=amogen, nbret = iret)
    if (iret.eq.0) lamor = 1
!   C-Matrix is supplied
    if (lamor.eq.0) then
        call jeveuo(amogen//'           .REFA', 'L', vk24=refa)
        nume24(1:14) = refa(2)(1:14)
!       --- Diagonal terms extraction and saving
        call dtminivec(sd_dtm, _AMOR_DIA, nbmode, address=jamo1)
        call extdia(amogen, nume24, substruc, zr(jamo1))
!       --- Testing of critical damping coefficient, 2 x sqrt(mi x ki)
        do i = 1, nbmode
            acrit = 2.d0*sqrt(abs(zr(jmas1+i-1)*zr(jrig1+i-1)))
            agene = zr(jamo1+i-1)
            if (agene .gt. acrit) then
                vali (1) = i
                valr (1) = agene
                valr (2) = acrit
                valk (1) = ' '
                call utmess('A', 'ALGORITH16_20', sk=valk(1), si=vali(1), nr=2,&
                            valr=valr)
            endif
        enddo
!
        nomstr = nume24(1:14)//'.SLCS'
        call jeveuo(nomstr(1:19)//'.SCDE', 'L', vi=scde)
        nbstor = scde(1)*scde(4)
        if (nbstor.gt.nbmode) then
!           --- Full matrix extraction and saving
            call dtminivec(sd_dtm, _AMOR_FUL, nbmode*nbmode, address=jamo2)
!           --- Fully damp "lagrange" dof in substructuring case
            if (lagrcorr.eq.1) then
                amotem = '&&29&AMO'
                call ajlagr(riggen, amogen, amotem)
                call copmat(amotem, zr(jamo2))
                call jeveuo(amotem//'           .CONL', 'E', vr=conl)
                do i = 1, nbmode
                    conl(i) = conl(i)/2.0d0
                end do               
            else
                call copmat(amogen, zr(jamo2))
            end if
        else
!           --- Diagonal matrix, multiply by the inverse of M
            do i=1, nbmode
                zr(jamo1+i-1) = zr(jamo1+i-1)/zr(jmas1+i-1)
            end do
        end if
        call dtmsav(sd_dtm, _AMOR_MAT,1, kscal=amogen)
!
    else
!       --- Creation of a diagonal damping matrix from the supplied damping coefs list
        call dtmprep_damp(sd_dtm)
    endif
!
!   -------------------------------------------------------------------------------------
!
!   --- 4.4 - Case of substructuring, saving matrices description pointers
    if (substruc.eq.1) then
        call mtdscr(riggen//'           ')
        call jeveuo(riggen//'           .&INT', 'E', descr)
        call mtdscr(mastem//'           ')
        call jeveuo(mastem//'           .REFA', 'E', jrefa)
        zk24(jrefa-1+7) = solver
        call jeveuo(mastem//'           .&INT', 'E', descm)
        if (lamor.eq.0) then
            call mtdscr(amotem//'           ')
            call jeveuo(amotem//'           .&INT', 'E', desca)
        else
            desca = 0
        endif
        call dtmsav(sd_dtm, _MAT_DESC,3,ivect=[descm,descr,desca])
    else
        call dtmsav(sd_dtm, _MAT_DESC,3,ivect=[0,0,0])
    end if

!
!   --------------------------------------------------------------------------------------
!   5 - Fluid Structure Interactions
!   --------------------------------------------------------------------------------------
!
    call dtmprep_fsi(sd_dtm)

!
!   --------------------------------------------------------------------------------------
!   6 - Information about the external charging (EXCIT and EXCIT_RESU)
!   --------------------------------------------------------------------------------------
!
!    NOTE : in 6.3 the call to mdrecf includes the VECT_ASSE_GENE
!           TODO : REMOVE VECT_GEN FROM SD_DTM PARAMS
!   --- 6.1 - Charging with a generalized vector : EXCIT -> VECT_ASSE_GENE
    call getfac('EXCIT', nexcit)
    if (nexcit .gt. 0) then
        ! call dtminivec(sd_dtm, _VECT_GEN, nexcit, address=jvec)
        ! do i = 1, nexcit
        !     call getvid('EXCIT', 'VECT_ASSE_GENE', iocc=i, scal=vecgen)
        !     zk8(jvec-1+i) = vecgen
        ! enddo
        continue
    else
        nexcit = 0
    end if

!
!   --- 6.2 - Charging from an existing result : EXCIT_RESU -> RESULTAT
    call getfac('EXCIT_RESU', nexcir)
    if (nexcir .gt. 0) then
        call dtminivec(sd_dtm, _VECT_RES, nexcir, address=jvecr)
        do i = 1, nexcir
            call getvid('EXCIT_RESU', 'RESULTAT', iocc=i, scal=resgen)
            zk8(jvecr-1+i) = resgen
!           --- Verify that the modal basis associated to the charging result is the same
!               as that corresponding to the projection basis of the M and K matrices
            call dismoi('BASE_MODALE', resgen, 'RESU_DYNA', repk=basemo2)
            if (basemo .ne. basemo2) then
                call utmess('F', 'ALGORITH17_18', si=i)
            endif
        enddo
    else
        nexcir = 0
    end if 
!
!   --- 6.3 - Additional excitation parameters
    ntotex = nexcit + nexcir*nbmode
    monmot = 'NON'
    if (ntotex .ne. 0) then
!       --- Function multiplicatory coefficient
        call dtminivec(sd_dtm, _COEF_MLT, ntotex, address=jcoefm)
!       --- Generalized vector .VALE jeveux address 
        call dtminivec(sd_dtm, _ADRES_VC, ntotex, address=jiadve)
!       --- Excited mode number (number of order)
        call dtminivec(sd_dtm, _N_ORD_VC, ntotex, address=jinumo)
!       --- Decriptive integer giving the nature of the charging (1-4)
!           1,2 : FONC_MULT/ACCE  -   1,3 : VECT_ASSE_GENE
!           3,4 : COEF_MULT       -   2,4 : NUME_ORDRE
        call dtminivec(sd_dtm, _DESC_FRC, ntotex, address=jidesc)
!       --- Function name, for FONC_MULT and/or ACCE
        call dtminivec(sd_dtm, _FUNC_NAM, 2*ntotex, address=jnomfo)
!
        call wkvect(calcres//'           .FDEP', 'G V K8', 2*ntotex, jnodep)
        call wkvect(calcres//'           .FVIT', 'G V K8', 2*ntotex, jnovit)
        call wkvect(calcres//'           .FACC', 'G V K8', 2*ntotex, jnoacc)
!
        call mdrecf(nexcit, nexcir, zi(jidesc), zk8(jnomfo), zr(jcoefm),&
                    zi(jiadve), zi(jinumo), zk8(jnodep), zk8(jnovit), zk8(jnoacc),&
                    neq, typco, basemo, nbmode, zr(jrig1),&
                    monmot, calcres)
!
        call jeexin(calcres//'           .IPSD', iret)
        if (iret .ne. 0) call jeveuo(calcres//'           .IPSD', 'E', jpsdel)
    endif
    call dtmsav(sd_dtm, _NB_EXC_T,1,iscal=ntotex)
    call dtmsav(sd_dtm, _MULTI_AP,1,kscal=monmot)

!   --------------------------------------------------------------------------------------
!   7 - Nonlinearities : (1) Chocs               / CHOC
!           <A>          (2) Anti sismic devices / ANTI_SISM 
!                        (3) Viscous dampers     / DIS_VISC
!                        (4) Buckling            / FLAMBAGE
!                        (5) Cracked rotor       / ROTOR_FISS
!                        (6) Lubrication         / COUPLAGE_EDYOS
!   --------------------------------------------------------------------------------------
    call dtmprep_noli(sd_dtm)
!
!   --------------------------------------------------------------------------------------
!   7 - Nonlinearities : (1) Gyroscopy           / MATR_GYRO, MATR_RIGY, VITESSE_VAR...
!           <B>          (2) F(X) relationship   / RELA_EFFO_DEPL
!                        (3) F(V) relationship   / RELA_EFFO_VITE
!   --------------------------------------------------------------------------------------
!
!   --- 7.1 - Gyroscopy
    vrotat = 0.d0
    call getvtx(' ', 'VITESSE_VARIABLE', scal=k8var)
    if (k8var(1:3) .eq. 'OUI') then
        call dtminivec(sd_dtm, _GYRO_FUL, nbmode*nbmode, address=jgyog)
        call getvid(' ', 'VITE_ROTA', scal=foncv)
        call getvid(' ', 'MATR_GYRO', scal=gyogen)
        call getvid(' ', 'ACCE_ROTA', scal=fonca)
        call getvid(' ', 'MATR_RIGY', scal=rgygen, nbret=ng2)
        call jeveuo(gyogen//'           .REFA', 'L', vk24=refag)
        numgeg = refag(2)(1:14)
        nomstg = numgeg//'.SLCS'
        call jeveuo(nomstg//'.SCDE', 'L', jscdeg)
        call copmat(gyogen, zr(jgyog))
        if (ng2 .ne. 0) then
            call dtminivec(sd_dtm, _RIGY_FUL, nbmode*nbmode, address=jrgyg)
            call copmat(rgygen, zr(jrgyg))
        else
            call dtminivec(sd_dtm, _RIGY_FUL, nbmode*nbmode, address=jrgyg)
            call r8inir(nbmode*nbmode, 0.d0, zr(jrgyg), 1)
            fonca = ' '
        endif
        call dtmsav(sd_dtm, _V_ROT_F, 1, kscal=foncv)
        call dtmsav(sd_dtm, _A_ROT_F, 1, kscal=fonca)       
    else
        call getvr8(' ', 'VITE_ROTA', scal=vrotat)
    endif
    call dtmsav(sd_dtm, _V_ROT, 1, rscal=vrotat)
!
!   --- 7.2 - RELA_EFFO_DEPL
    call getfac('RELA_EFFO_DEPL', nbrede)
    if (nbrede .gt. 0) then
        call dtminivec(sd_dtm, _FX_DEPLR, nbrede*6*nbmode, address=jrede)
        call dtminivec(sd_dtm, _FX_FONCT, nbrede*3, address=jfond)
        call mdrede(numddl, nbrede, nbmode, zr(jbase), neq,&
                    zr(jrede), zk8(jfond), iret)
        if (iret .ne. 0) call utmess('F', 'ALGORITH5_24')
    else
        nbrede = 0
    end if
    call dtmsav(sd_dtm, _FX_NUMB, 1, iscal=nbrede)
!
!   --- 7.3 - RELA_EFFO_VITE
    call getfac('RELA_EFFO_VITE', nbrevi)
    if (nbrevi .gt. 0) then
        call dtminivec(sd_dtm, _FV_DEPLR, nbrevi*6*nbmode, address=jrevi)
        call dtminivec(sd_dtm, _FV_FONCT, nbrevi*3, address=jfonv)
        call mdrevi(numddl, nbrevi, nbmode, zr(jbase), neq,&
                    zr(jrevi), zk8(jfonv), iret)
        if (iret .ne. 0) call utmess('F', 'ALGORITH5_24')
    else
        nbrevi = 0
    end if
    call dtmsav(sd_dtm, _FV_NUMB, 1, iscal=nbrevi)
!
!   --------------------------------------------------------------------------------------
!   8 - Integration duration, time steps (mini/maxi)
!   --------------------------------------------------------------------------------------
!
    call dtmget(sd_dtm, _NB_NONLI, iscal=nbnli)
    lisins = '&&DTMPREP'

    if (nbnli.gt.0) then
        call dtmget(sd_dtm, _CHO_DEPL, vr=chodep)
        call dtmget(sd_dtm, _CHO_PARA, vr=chopar)
        call dtmget(sd_dtm, _CHO_NOEU, vk8=chonoe)
!
        call mdptem(nbmode, zr(jmas1), puls, nbnli, chodep,&
                    chopar, chonoe, dt, dtmax, dtmin,&
                    tinit, tfin, nbpas, iret, lisins)
    else 
        call mdptem(nbmode, zr(jmas1), puls, nbnli, [0.d0],&
                    [0.d0], [' '], dt, dtmax, dtmin,&
                    tinit, tfin, nbpas, iret, lisins)
    endif
    if (iret .ne. 0) call utmess('F', 'ALGORITH5_24')
    call jedetr(lisins)
!
    AS_DEALLOCATE(vr=puls )
!
!
    call dtmsav(sd_dtm, _DT      , 1, rscal=dt)
    call dtmsav(sd_dtm, _DT_MIN  , 1, rscal=dtmin)
    call dtmsav(sd_dtm, _DT_MAX  , 1, rscal=dtmax)
    call dtmsav(sd_dtm, _INST_INI, 1, rscal=tinit)
    call dtmsav(sd_dtm, _INST_FIN, 1, rscal=tfin)
    call dtmsav(sd_dtm, _NB_STEPS, 1, iscal=nbpas)
!
!   --- If needed, initialiaze EDYOS coupling with time-step information
    call dtmget(sd_dtm, _NB_PALIE, iscal=nbpal)
    if (nbpal.ne.0) then
        call dtmget(sd_dtm, _DT_EDYOS, rscal=dtedyo)
        call inicou(nbpas, tinit, tfin, dt, dtedyo,&
                    vrotat)
    endif
!
!   --------------------------------------------------------------------------------------
!   9 - Saving periodicity (ARCHIVAGE)
!   --------------------------------------------------------------------------------------
!
    call dtmprep_arch(sd_dtm)
!
!
!   --------------------------------------------------------------------------------------
!   10 - Memory allocation of the result
!   --------------------------------------------------------------------------------------
!
    call dtmallo(sd_dtm)
!
    call jedema()
end subroutine
