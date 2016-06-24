subroutine dtmprep_noli(sd_dtm_)
    implicit none
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! dtmprep_noli : Retreives information regarding 6 types of nonlinearities for a
!                DYNA_VIBRA//TRAN/GENE calculation :
!                (1)    Chocs                               / CHOC
!                (2)    Anti sismic devices                 / ANTI_SISM
!                (3.1)  Viscous dampers                     / DIS_VISC
!                (3.2)  Springs with isotropic behavior     / DIS_ECRO_TRAC
!                (4)    Buckling                            / FLAMBAGE
!                (5)    Cracked rotor                       / ROTOR_FISS
!                (6)    Lubrication                         / COUPLAGE_EDYOS
!
!   Note : Information about these 6 nonlinearity types are read using mdchoc
!
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterc/getfac.h"
#include "asterfort/copisd.h"
#include "asterfort/cricho.h"
#include "asterfort/dismoi.h"
#include "asterfort/dtmcase_coder.h"
#include "asterfort/dtmget.h"
#include "asterfort/dtminivec.h"
#include "asterfort/dtmsav.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/lecdon.h"
#include "asterfort/mdchoc.h"
#include "asterfort/mdtr74grd.h"
#include "asterfort/r8inir.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
!   -0.1- Input/output arguments
    character(len=*)          , intent(in) :: sd_dtm_
!
!   -0.2- Local variables
    aster_logical    :: okschema, prdeff, lamor
    integer          :: nbcho1, ioc, nbchoc, n1, n2
    integer          :: ngr, nbmg, nbsism(3), nbflam, nbrfis
    integer          :: jfk, jdfk, lprol, nbedyo, nbpal
    integer          :: unitpa, iadri, nbnli, jranc, jdepl
    integer          :: jparc, jinti, jnoec, ntotex, nbexcit, jpain
    integer          :: jpsid, iret, info, neq, ifm
    integer          :: ig, iadrk, palmax, i, jpsdel
    integer          :: nbmode, ivchoc, nbschor, j, nlcase
    integer          :: jrefa, jbid
    real(kind=8)     :: angini, rad, dtsto, seuil, crit
    character(len=1) :: niv
    character(len=7) :: casek7
    character(len=8) :: sd_dtm, mailla, k8var, foncp, monmot
    character(len=8) :: vercho
    character(len=14):: numddl
    character(len=16):: valk(3), nltreat_k, schema
    character(len=19):: marig, nomres, fonct, rigass
    character(len=24):: solver
!
    real(kind=8)     , pointer :: basvec(:)   => null()
    real(kind=8)     , pointer :: fsauv(:)    => null()
    real(kind=8)     , pointer :: puls(:)     => null()
    real(kind=8)     , pointer :: riggen(:)   => null()
    real(kind=8)     , pointer :: masgen(:)   => null()
    real(kind=8)     , pointer :: amogen(:)   => null()
    real(kind=8)     , pointer :: basev0(:)   => null()
    real(kind=8)     , pointer :: amored(:)   => null()

    integer          , pointer :: indicv(:)    => null()
    real(kind=8)     , pointer :: fimpv(:)     => null()
    real(kind=8)     , pointer :: rfimpv(:)    => null()
    real(kind=8)     , pointer :: souplv(:)    => null()
    real(kind=8)     , pointer :: trlocv(:)    => null()

    character(len=8) , pointer :: typal(:)    => null()
    character(len=8) , pointer :: finpal(:)   => null()
    character(len=8) , pointer :: cnpal(:)    => null()
    character(len=24), pointer :: group_ma(:) => null()
!
#define base0(row,col) basev0((row-1)*nbmode+col)
!
!
!   -0.3- Initializations
    call jemarq()
    call infmaj()
    call infniv(ifm, info)

    sd_dtm = sd_dtm_
    rad = r8dgrd()
!
!   -0.4- Retrieval of the necessary information
    call dtmget(sd_dtm, _AMOR_FUL, lonvec=iret)
    if (iret.eq.0) then
        lamor = .true.
        call dtmget(sd_dtm, _AMOR_DIA, vr=amogen)
    else
        lamor = .false.
        call dtmget(sd_dtm, _AMOR_FUL, vr=amogen)
    end if
!
    call dtmget(sd_dtm, _NB_EXC_T,iscal=ntotex)
!
    call dtmget(sd_dtm, _RIGI_DIA, vr=riggen)
    call dtmget(sd_dtm, _MASS_DIA, vr=masgen)
!
    call dtmget(sd_dtm, _SCHEMA  , kscal=schema)
    call dtmget(sd_dtm, _NUM_DDL , kscal=numddl)
    call dismoi('NOM_MAILLA', numddl , 'NUME_DDL', repk=mailla)

!
!   --- 1 - CHOC
    call getfac('CHOC', nbcho1)
    nbchoc = 0

    do ioc = 1, nbcho1
        call getvtx('CHOC', 'MAILLE', iocc=ioc, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            nbchoc = nbchoc - n1
        else
            call getvtx('CHOC', 'GROUP_MA', iocc=ioc, nbval=0, nbret=n2)
            if (n2 .ne. 0) then
                ngr = -n2
                AS_ALLOCATE(vk24=group_ma, size=ngr)
                call getvtx('CHOC', 'GROUP_MA', iocc=ioc, nbval=ngr, vect=group_ma)
                do ig = 0, ngr-1
                    call jelira(jexnom(mailla//'.GROUPEMA', group_ma(ig+1)), 'LONMAX', nbmg)
                    nbchoc = nbchoc + nbmg
                enddo
                AS_DEALLOCATE(vk24=group_ma)
            else
                nbchoc = nbchoc + 1
            endif
        endif
    enddo
    call dtmsav(sd_dtm, _NB_CHOC ,1, iscal=nbchoc)

!
!   --- 2 - ANTI_SISM
    nbsism(1:3)= 0
    call getfac('ANTI_SISM', nbsism(1))
    if (nbsism(1) .ne. 0) then
        okschema =     (schema(1:11).eq.'DIFF_CENTRE').or.(schema(1:6).eq.'DEVOGE')&
                   .or.(schema(1:5).eq.'RUNGE').or.(schema(1:5).eq.'ADAPT')
        if (.not. okschema) then
            valk(1) = 'ANTI_SISM'
            valk(2) = schema
            call utmess('A', 'ALGORITH5_81', nk=2, valk=valk)
        endif
    endif
    call dtmsav(sd_dtm, _NB_ANTSI,1,iscal=nbsism(1))
!
!   --- 3.1 - DIS_VISC
    call getfac('DIS_VISC', nbsism(2))
    if (nbsism(2) .ne. 0) then
        okschema = (schema(1:11).eq.'DIFF_CENTRE').or.(schema(1:5).eq.'RUNGE')
        if (.not. okschema) then
            valk(1) = 'DIS_VISC'
            valk(2) = schema
            call utmess('A', 'ALGORITH5_81', nk=2, valk=valk)
        endif
    endif
    call dtmsav(sd_dtm, _NB_DIS_VISC,1,iscal=nbsism(2))
!
!   --- 3.2 - DIS_ECRO_TRAC
    call getfac('DIS_ECRO_TRAC', nbsism(3))
    if (nbsism(3) .ne. 0) then
        okschema = (schema(1:11).eq.'DIFF_CENTRE').or.(schema(1:5).eq.'RUNGE')
        if (.not. okschema) then
            valk(1) = 'DIS_ECRO_TRAC'
            valk(2) = schema
            call utmess('A', 'ALGORITH5_81', nk=2, valk=valk)
        endif
    endif
    call dtmsav(sd_dtm, _NB_DIS_ECRO_TRAC,1,iscal=nbsism(3))
!
!   --- 4 - FLAMBAGE
    call getfac('FLAMBAGE', nbflam)
    call dtmsav(sd_dtm, _NB_FLAMB,1,iscal=nbflam)
!
!   --- 5 - ROTOR_FISS
    nbrfis = 0
    angini = 0.d0
    call getfac('ROTOR_FISS', nbrfis)
    call getvtx(' ', 'VITESSE_VARIABLE', scal=k8var)
    if (nbrfis .ne. 0) then
!        if (schema .ne. 'EULER') then
!            call utmess('F', 'ALGORITH5_80')
!        endif

        call dtminivec(sd_dtm, _ROTR_FK , 2*nbrfis, address=jfk)
        call dtminivec(sd_dtm, _ROTR_DFK, 2*nbrfis, address=jdfk)
        do ioc = 1, nbrfis
            call getvid('ROTOR_FISS', 'K_PHI', iocc=ioc, scal=fonct)
            call jeveuo(fonct//'.PROL', 'L', lprol)
            zk8(jfk)   = fonct(1:8)
            zk8(jfk+1) = zk24(lprol)(1:8)
            call getvid('ROTOR_FISS', 'DK_DPHI', iocc=ioc, scal=fonct)
            call jeveuo(fonct//'.PROL', 'L', lprol)
            zk8(jdfk)   = fonct(1:8)
            zk8(jdfk+1) = zk24(lprol)(1:8)
            if (k8var(1:3) .eq. 'OUI') then
                call getvid('ROTOR_FISS', 'ANGL_ROTA', iocc=ioc, scal=foncp)
                call dtmsav(sd_dtm, _ANGL_FON,1, kscal=foncp)
            else
                call getvr8('ROTOR_FISS', 'ANGL_INIT', iocc=ioc, scal=angini)
                angini=angini*rad
                call dtmsav(sd_dtm, _ANGL_INI,1,rscal=angini)
            endif
        enddo
    endif
    call dtmsav(sd_dtm, _NB_R_FIS,1,iscal=nbrfis)
!
!   --- 6 - COUPLAGE_EDYOS
    nbedyo = 0
    call getfac('COUPLAGE_EDYOS', nbedyo)
    nbpal = 0
    dtsto = 0.d0
    if (nbedyo .ne. 0) then
        call getfac('PALIER_EDYOS', nbedyo)
        if (nbedyo .ne. 0) then
!           Lecture des donnees paliers
            call getvis('PALIER_EDYOS', 'UNITE', iocc=1, scal=unitpa, nbret=n1)
            if (n1 .ne. 0) then
                call lecdon(.true._1, unitpa, prdeff)
            else
                call lecdon(.false._1, 0, prdeff)
            endif
            call getvr8('COUPLAGE_EDYOS', 'PAS_TPS_EDYOS', iocc=1, scal=dtsto)
        else
            call utmess('F', 'EDYOS_48')
        endif
        call dtmsav(sd_dtm, _DT_EDYOS,1,rscal=dtsto)

        call jeveuo('N_PAL', 'L', iadri)
        nbpal=zi(iadri)

        call jeveuo('C_PAL', 'L', iadrk)
        palmax = 20
        call dtminivec(sd_dtm, _PAL_TYP , nbpal, vk8=typal)
        call dtminivec(sd_dtm, _PAL_FIN , nbpal, vk8=finpal)
        call dtminivec(sd_dtm, _PAL_CN  , nbpal, vk8=cnpal)
        call dtminivec(sd_dtm, _PAL_FSAV, palmax*3, vr=fsauv)
        call r8inir(palmax*3, 0.d0, fsauv, 1)

        do i = 1, nbpal
            typal(i)=zk8(iadrk+(i-1))(1:6)
            finpal(i)=zk8(iadrk+(i-1)+palmax)(1:3)
            cnpal(i)=zk8(iadrk+(i-1)+2*palmax)(1:8)
        enddo
        nbnli = 0
        call dtmsav(sd_dtm, _PAL_NBCV,2,ivect=[0,10])
    endif
    call dtmsav(sd_dtm, _NB_PALIE,1,iscal=nbpal)
!
!   --- Total number of nonlinearities
    nbnli = nbchoc + nbsism(1) + nbsism(2) + nbsism(3) + nbflam + nbrfis + nbpal
    call dtmsav(sd_dtm, _NB_NONLI, 1, iscal=nbnli)

!   Reading and processing information for these nonlinearities <A> (1-6)
    call dtmsav(sd_dtm, _NL_TREAT, 1, iscal=0)
    if (nbnli .ne. 0) then

        call dtmget(sd_dtm, _NB_MODES,iscal=nbmode)

!       --- Explicit or implicit treatment of choc non-linearities
        if (nbchoc.gt.0) then
            call getvtx(' ', 'TRAITEMENT_NONL', iocc=1, scal=nltreat_k)
            if (nltreat_k(1:9).eq.'IMPLICITE') then

                if (nbchoc.gt.41) call utmess('F', 'DYNAMIQUE_28', si=41)

                call dtmsav(sd_dtm, _NL_TREAT, 1, iscal=1)
                call dtminivec(sd_dtm, _F_NL_ADD, nbmode)
                call dtminivec(sd_dtm, _IMP_DEPL, nbmode)
                call dtminivec(sd_dtm, _IMP_VITE, nbmode)
                call dtminivec(sd_dtm, _IMP_ACCE, nbmode)
                call dtminivec(sd_dtm, _IMP_FEXT, nbmode)

                nlcase = 0
                call dtmcase_coder (nlcase, casek7)
                call wkvect(sd_dtm // '.PRJ_BAS.'//casek7, 'V V R', nbmode*nbmode, vr=basev0)
                do i = 1, nbmode
                    base0(i,i) = 1.d0
                    do j = i+1, nbmode
                        base0(i,j) = 0.d0
                    end do
                end do

                nbschor = nbnli*(mdtr74grd('SCHOR')+mdtr74grd('MAXVINT'))
                call dtminivec(sd_dtm, _NL_SAVE0, nbschor)
            end if
        end if

        call dtmget(sd_dtm, _NB_PHYEQ, iscal=neq)
        call dtminivec(sd_dtm, _CHO_RANK, nbnli*mdtr74grd('LOGCHO'), address=jranc)
        call dtminivec(sd_dtm, _CHO_DEPL, nbnli*6*nbmode, address=jdepl)
        call dtminivec(sd_dtm, _CHO_PARA, nbnli*mdtr74grd('PARCHO'),  address=jparc)
        call dtminivec(sd_dtm, _CHO_PAIN, nbnli*mdtr74grd('PAINCHO'), address=jpain)
        call dtminivec(sd_dtm, _CHO_NAME, nbnli, address=jinti)
        call dtminivec(sd_dtm, _CHO_NOEU, nbnli*9, address=jnoec)
        call dtminivec(sd_dtm, _F_TOT_WK, nbmode, address=jbid)
        call dtminivec(sd_dtm, _F_TAN_WK, nbmode, address=jbid)

        if (ntotex .ne. 0) then
            nbexcit = ntotex
            call dtminivec(sd_dtm, _PSI_DELT, nbnli*6*ntotex, address=jpsid)
        else
            nbexcit = 1
        endif
!
        call dtmget(sd_dtm, _MULTI_AP, kscal=monmot)
        call dtmget(sd_dtm, _CALC_SD , kscal=nomres)
        call dtmget(sd_dtm, _BASE_VEC, vr=basvec)
        call dtmget(sd_dtm, _OMEGA   , vr=puls)

        jpsdel = 1
        call jeexin(nomres//'.IPSD', iret)
        if (iret .ne. 0) call jeveuo(nomres//'.IPSD', 'E', jpsdel)


        if (lamor) then
            AS_ALLOCATE(vr=amored, size=nbmode)
            do i=1, nbmode
                if (abs(puls(i)).lt. 1.D-10) then 
                    amored = 0.D0
                else
                    amored = amogen(i)/(2.d0*puls(i))
                endif
            end do
        end if
        call mdchoc(nbnli, nbchoc, nbflam, nbsism, nbrfis,&
                    nbpal, zi(jranc), zr(jdepl), zr(jparc), zi(jpain), zk8(jnoec),&
                    zk8(jinti), zr(jpsdel), zr(jpsid), numddl, nbmode,&
                    puls, masgen, lamor, amored, basvec,&
                    neq, nbexcit, info, monmot(1:8), iret)
        if (iret .ne. 0) call utmess('F', 'ALGORITH5_24')
        AS_DEALLOCATE(vr=amored)
!
        call getfac('VERI_CHOC', ivchoc)
        if (ivchoc .ne. 0) then

            AS_ALLOCATE(vr=fimpv, size=neq)
            AS_ALLOCATE(vr=rfimpv, size=neq)
            AS_ALLOCATE(vr=souplv, size=nbmode)
            AS_ALLOCATE(vr=trlocv, size=nbmode)
            AS_ALLOCATE(vi=indicv, size=nbmode)
!
            call dtmget(sd_dtm, _RIGI_MAT, kscal=rigass)
            call dtmget(sd_dtm, _SOLVER, savejv=solver)
            marig = '&&OP0029.RIGI'
            call copisd('MATR_ASSE', 'V', rigass, marig)
            call jeexin(marig//'.REFA', iret)
            if (iret .eq. 0) call wkvect(marig//'.REFA', 'V V K24', 20, jrefa)
            call jeveuo(marig//'.REFA', 'E', jrefa)
            zk24(jrefa-1+7)=solver
!
            call cricho(nbmode, riggen, nbnli-nbpal, zr(jparc), zk8(jnoec),&
                        info, fimpv, rfimpv, trlocv, souplv,&
                        indicv, neq, basvec, seuil, marig,&
                        nbnli-nbpal)
!
            AS_DEALLOCATE(vr=fimpv)
            AS_DEALLOCATE(vr=rfimpv)
            AS_DEALLOCATE(vr=souplv)
            AS_DEALLOCATE(vr=trlocv)
            AS_DEALLOCATE(vi=indicv)
!
            call getvr8('VERI_CHOC', 'SEUIL', iocc=1, scal=crit)
            if (seuil .gt. crit) then
                niv = 'A'
                call getvtx('VERI_CHOC', 'STOP_CRITERE', iocc=1, scal=vercho)
                if (vercho(1:3) .eq. 'OUI') niv = 'F'
                call utmess('I', 'ALGORITH16_21', sr=seuil)
                call utmess(niv, 'ALGORITH5_66')
            endif
        endif
    endif

    call jedema()
end subroutine
