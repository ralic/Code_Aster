subroutine dtmprep_verichoc(sd_dtm_, sd_nl_)
    implicit none
! ----------------------------------------------------------------------
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
!
!     DYNA_ VIBRA : Calculation and priniting out of the restitution rates 
!                    for stop/choc -type localized non linearities
! ----------------------------------------------------------------------
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/calsvd.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/dtmget.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdtrib.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtdscr.h"
#include "asterfort/nlget.h"
#include "asterfort/posddl.h"
#include "asterfort/preres.h"
#include "asterfort/resoud.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "blas/ddot.h"
!
    character(len=*), intent(in):: sd_dtm_
    character(len=*), intent(in):: sd_nl_
!
    aster_logical :: matuv
!
    integer :: nbchoc, info, nbmode, irigi, ibid
    integer :: nl_type, vali, jrefa, jmod, ifm
    integer :: neq, nbnli, istoav, iret, ifac
    integer :: i, j, jj, k, ia
    integer :: jm, iddlx, iddly, iddlz, nunoe
    integer :: nm, m, n, ierr, icolc
    integer :: nbch1, nbch2, neqch1, neqch2
!
    real(kind=8) :: seuil, crit, trlocj, valr(3), nvect(3)
    real(kind=8) :: soup, cef, kn, cc, cs
    real(kind=8) :: ct, scf, rscf, usr, normx
    real(kind=8) :: mmax, mmin, scond, eps
!
    complex(kind=8) :: cbid
!
    character(len=1)  :: niv
    character(len=8)  :: sd_dtm, sd_nl, nume, noeu, nume1
    character(len=8)  :: noeu1, obst_typ, nume2, noeu2
    character(len=8)  :: rigass, vercho
    character(len=19) :: marig, solveu, matpre
    character(len=24) :: solver
!
    integer     , pointer :: indic(:) => null()
    real(kind=8), pointer :: riggen(:) => null()
    real(kind=8), pointer :: a(:) => null()
    real(kind=8), pointer :: efloc(:) => null()
    real(kind=8), pointer :: normxx(:) => null()
    real(kind=8), pointer :: normy(:) => null()
    real(kind=8), pointer :: u(:) => null()
    real(kind=8), pointer :: v(:) => null()
    real(kind=8), pointer :: w(:) => null()
    real(kind=8), pointer :: trloc(:) => null()
    real(kind=8), pointer :: restit(:) => null()
    real(kind=8), pointer :: soupl(:) => null()
    real(kind=8), pointer :: fimpo(:) => null()
    real(kind=8), pointer :: rfimpo(:) => null()
    real(kind=8), pointer :: rfimpx(:) => null()

    integer, pointer :: slvi(:) => null()
!
#define bmodal(m,n) zr(jmod-1+(n-1)*neq+m)
!
!   --- 0 - Initialization
    sd_dtm = sd_dtm_
    sd_nl  = sd_nl_
!
    eps    = r8prem( )
    seuil  = 0.d0
    cbid   = dcmplx(0.d0, 0.d0)
    ifac   = 0
    icolc  = 0
!-----------------------------------------------------------------------
    call infmaj()
    call infniv(ifm, info)
!-----------------------------------------------------------------------
    call dtmget(sd_dtm, _NB_MODES, iscal=nbmode)
    call dtmget(sd_dtm, _NB_NONLI, iscal=nbnli)
    call dtmget(sd_dtm, _RIGI_MAT, kscal=rigass)
    call dtmget(sd_dtm, _SOLVER  , savejv=solver)
    call dtmget(sd_dtm, _RIGI_DIA, vr=riggen)
    call dtmget(sd_dtm, _BASE_VEC, address=jmod)
    call dtmget(sd_dtm, _NB_PHYEQ, iscal=neq)
    call nlget (sd_nl , _NB_CHOC , iscal=nbchoc)
    if (nbchoc.eq.0) goto 999

    solveu = solver(1:19)
!
!   --- Prepare the rigidity matrix
    marig = '&&OP0029.RIGI'
    call copisd('MATR_ASSE', 'V', rigass, marig)
    call jeexin(marig//'.REFA', iret)
    if (iret .eq. 0) call wkvect(marig//'.REFA', 'V V K24', 20, jrefa)
    call jeveuo(marig//'.REFA', 'E', jrefa)
    zk24(jrefa-1+7)=solveu
    call mtdscr(marig)
    call jeveuo(marig//'.&INT', 'E', irigi)

    AS_ALLOCATE(vr=fimpo , size=neq)
!
    nbch1  = 1+2*nbchoc
    nbch2  = 2*nbchoc
    neqch1 = neq*nbch1
    neqch2 = neq*nbch2
!
    AS_ALLOCATE(vr=rfimpo, size=neq)
    AS_ALLOCATE(vr=rfimpx, size=2*neq*nbchoc)
    AS_ALLOCATE(vr=efloc , size=nbmode)
    AS_ALLOCATE(vr=normy , size=nbmode)
    AS_ALLOCATE(vr=normxx, size=2*nbchoc)
    AS_ALLOCATE(vr=trloc , size=nbmode)
    AS_ALLOCATE(vr=soupl , size=nbmode)
    AS_ALLOCATE(vr=restit, size=2*nbchoc)
    AS_ALLOCATE(vi=indic , size=nbmode)

    AS_ALLOCATE(vr=w     , size=1+2*nbchoc)
    AS_ALLOCATE(vr=a     , size=neq*(1+2*nbchoc))
    AS_ALLOCATE(vr=u     , size=neq*(1+2*nbchoc))
    AS_ALLOCATE(vr=v     , size=neq*(1+2*nbchoc))

!
    do i =1, nbnli
        call nlget(sd_nl, _NL_TYPE, iocc=i, iscal=nl_type)
        if (nl_type.eq.NL_CHOC) then
            call nlget(sd_nl, _NUMDDL_1, iocc=i, kscal=nume1)
            call nlget(sd_nl, _NO1_NAME, iocc=i, kscal=noeu1)

            call nlget(sd_nl, _OBST_TYP, iocc=i, kscal=obst_typ)
            if (obst_typ(1:2).eq.'BI') then
                call nlget(sd_nl, _NUMDDL_2, iocc=i, kscal=nume2)
                call nlget(sd_nl, _NO2_NAME, iocc=i, kscal=noeu2)
                jm = 2
            end if
            nume = nume1
            noeu = noeu1
            do jj = 1, jm
                if (jj.eq.2) then
                    nume = nume2
                    noeu = noeu2
                end if
                icolc = icolc+1
                ct=0.d0
                cef=0.d0
                call utmess('I', 'VIDE_1')
                if (info .ge. 2) call utmess('I', 'SOUSTRUC_85', sk=noeu)
                call utmess('I', 'VIDE_1')

                call vecini(neq, 0.d0, fimpo)
                call posddl('NUME_DDL', nume, noeu, 'DX', nunoe, iddlx)
                call posddl('NUME_DDL', nume, noeu, 'DY', nunoe, iddly)
                call posddl('NUME_DDL', nume, noeu, 'DZ', nunoe, iddlz)
                call nlget(sd_nl, _NORMAL_VECTOR, iocc=i, rvect=nvect)

                fimpo(iddlx) = nvect(1)
                fimpo(iddly) = nvect(2)
                fimpo(iddlz) = nvect(3)

!               --- Calculation of RFIMPO = K*n
                call mrmult('ZERO', irigi, fimpo, rfimpo, 1,&
                            .true._1)

!               --- First pass, prepare for K^-1 by calling preres
                if (ifac .eq. 0) then
                    call dismoi('SOLVEUR', marig, 'MATR_ASSE', repk=solveu)
                    call jeveuo(solveu//'.SLVI', 'E', vi=slvi)
                    matpre  = '&&OP0029.BIDON'
                    istoav  = slvi(3)
                    slvi(3) = 2
                    call preres(solveu, 'V', iret, matpre, marig,&
                                ibid, - 9999)
                    slvi(3) = istoav
                    if (iret .eq. 2) then
                        call utmess('A', 'SOUSTRUC_7')
                        goto 999
                    else if (iret.eq.1) then
                        call utmess('A', 'SOUSTRUC_8')
                        goto 999
                    endif
                    ifac=1
                endif

!               --- Calculation of FIMPO, static deformation : K^-1 * n
                call resoud(marig, ' ', solveu, ' ', 1,&
                            ' ', ' ', ' ', fimpo, [cbid],&
                            ' ', .true._1, 0, iret)

!               --- Calculation of NORMX, square magnitude of K^-1 * n
                normx = ddot(neq, fimpo, 1, fimpo,1)
                normxx(icolc)=normx

                do k = 1, neq
                    rfimpx(k+neq*(icolc-1))=fimpo(k)
                end do

                soup = nvect(1)*fimpo(iddlx)
                soup = soup + nvect(2)*fimpo(iddly)
                soup = soup + nvect(3)*fimpo(iddlz)
                call vecini(neq, 0.d0, fimpo)
                fimpo(iddlx) = nvect(1)
                fimpo(iddly) = nvect(2)
                fimpo(iddlz) = nvect(3)

                do j = 1, nbmode
                    if (riggen(j) .le. 0.d0) then
                        usr=0.d0
                    else
                        usr=1.d0/riggen(j)
                    endif

                    rscf = ddot(neq, bmodal(1,j), 1, rfimpo, 1)
                    scf  = ddot(neq, bmodal(1,j), 1, fimpo , 1)
                    cc = scf*rscf*usr
                    cs = scf**2*usr
                    if (info .ge. 2) then
                        soupl(j)=cs
                        if (abs(soup).gt.eps) then
                            trloc(j)=cs/soup
                        else
                            trloc(j)=0.d0
                        endif
                        indic(j) = j
                        trlocj   = trloc(j)
                    else
                        if (abs(soup).gt.eps) then
                            trlocj = cs/soup
                        else
                            trlocj = 0.d0
                        endif
                    endif
                    ct       = ct + trlocj
                    efloc(j) = cc
                    cef = cef + cc
                end do

!               --- Save the restitution ratio for this node
                restit((i-1)*2+jj) = ct

!               --- Print out the flexibilities (souplesses) in increasing order 
                if (info .ge. 2) then
                    call mdtrib(indic, soupl, nbmode)
                    do j = 1, nbmode
                        vali     = indic(j)
                        valr (1) = trloc(indic(j))
                        valr (2) = soupl(indic(j))
                        valr (3) = efloc(indic(j))
                        call utmess('I', 'SOUSTRUC_93', si=vali, nr=3, valr=valr)
                    end do
                endif

                call utmess('I', 'SOUSTRUC_94', sk=noeu, nr=2, valr=[ct, cef])
                call nlget(sd_nl, _STIF_NORMAL, iocc=i, rscal=kn)
                call utmess('I', 'SOUSTRUC_95', sr=soup*kn*(1.d0-ct))
                call utmess('I', 'SOUSTRUC_96', sr=soup*ct*kn)
                seuil = max(seuil, soup*kn*(1.d0-ct))
            end do
!
        end if
    end do
!
    if (info .ge. 2) then
        call utmess('I', 'VIDE_1')
        matuv = .false.
        nm = neq
        m  = neq
!
!       LA MATRICE A CONTIENT LES DEFORMEES STATIQUES
!       ICOLC : NB DE CHOC A CONSIDERER
        n = icolc
        do k = 1, neq
            do ia = 1, icolc
                if (normxx(ia) .gt. eps) then
                    a(k+neq*(ia-1)) = rfimpx(k+neq*(ia- 1))/sqrt(normxx(ia))
                else
                    a(k+neq*(ia-1)) = 0.d0
                endif
            end do
        end do
!
        call calsvd(nm, m, n, a, w,&
                    matuv, u, matuv, v, ierr)
        if (ierr .ne. 0) goto 999
        mmax = 0.d0
        mmin = 1.d10
        do ia = 1, n
            mmax = max(mmax,w(ia))
            mmin = min(mmin,w(ia))
        end do
! CONDITIONNEMENT
        if (mmin .le. eps) then
            valr (1) = mmin
            valr (2) = eps
            call utmess('I', 'SOUSTRUC_98', nr=2, valr=valr)
            mmin = eps
        endif
        scond = mmax/mmin
!
        valr (1) = scond
        call utmess('I', 'SOUSTRUC_99', sr=valr(1))
        do jj = 1, nbmode
            normy(jj)=ddot(neq, bmodal(1,jj), 1, bmodal(1,jj), 1)
        end do
!
        n = icolc+1
        do j = 1, nbmode
!
! LA MATRICE A CONTIENT LES DEFORMEES STATIQUES ET MODE
            do k = 1, neq
                do ia = 1, icolc
                    if (normxx(ia) .gt. eps) then
                        a(k+neq*(ia-1)) = rfimpx(k+neq* (ia-1))/sqrt(normxx(ia))
                    else
                        a(k+neq*(ia-1)) = 0.d0
                    endif
                end do
                a(k+neq*(icolc+1-1)) = bmodal(k,j)/sqrt(normy(j))
            end do
!
            call calsvd(nm, m, n, a, w,&
                        matuv, u, matuv, v, ierr)
            if (ierr .ne. 0) goto 999
            mmax = 0.d0
            mmin = 1.d10
            do ia = 1, n
                mmax = max(mmax,w(ia))
                mmin = min(mmin,w(ia))
            end do
! CONDITIONNEMENT
            if (mmin .le. eps) then
                vali = j
                valr (1) = mmin
                valr (2) = eps
                call utmess('I', 'SOUSTRUC2_1', si=vali, nr=2, valr=valr)
                mmin = eps
            endif
            efloc(j) = mmax/mmin
! NORMALISATION PAR RAPPORT DEF STATIQUE
            if (scond .le. eps) scond = eps
            efloc(j) = efloc(j)/scond
!
            indic(j)=j
        end do
!
!      ON ORDONNE SELON LA PARTICIPATION DECROISSANTE
        call mdtrib(indic, efloc, nbmode)
        do j = 1, nbmode
            vali = indic(j)
            valr (1) = efloc(indic(j))
            call utmess('I', 'SOUSTRUC2_2', si=vali, sr=valr(1))
        end do
!
        call utmess('I', 'VIDE_1')
    endif
!
! --- MENAGE
!
    AS_DEALLOCATE(vr=rfimpo)
    AS_DEALLOCATE(vr=fimpo )
    AS_DEALLOCATE(vr=rfimpx)
    AS_DEALLOCATE(vr=efloc )
    AS_DEALLOCATE(vr=normy )
    AS_DEALLOCATE(vr=normxx)
    AS_DEALLOCATE(vr=trloc )
    AS_DEALLOCATE(vr=restit)
    AS_DEALLOCATE(vi=indic )
    AS_DEALLOCATE(vr=w     )
    AS_DEALLOCATE(vr=a     )
    AS_DEALLOCATE(vr=u     )
    AS_DEALLOCATE(vr=v     )
    AS_DEALLOCATE(vr=soupl )

!
    call getvr8('VERI_CHOC', 'SEUIL', iocc=1, scal=crit)
    if (seuil .gt. crit) then
        niv = 'A'
        call getvtx('VERI_CHOC', 'STOP_CRITERE', iocc=1, scal=vercho)
        if (vercho(1:3) .eq. 'OUI') niv = 'F'
        call utmess('I', 'ALGORITH16_21', sr=seuil)
        call utmess(niv, 'ALGORITH5_66')
    endif


999 continue
!
end subroutine
