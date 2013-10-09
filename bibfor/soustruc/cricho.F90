subroutine cricho(nbmode, riggen, nbchoc, parcho, noecho,&
                  info, fimpo, rfimpo, trloc, soupl,&
                  indic, neq, bmodal, seuil, marig,&
                  nbnli)
    implicit none
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CALCUL DES TAUX DE RECONSTITUTION
!     ------------------------------------------------------------------
! IN  : NBMODE : NOMBRE DE MODES
! IN  : RIGGEN : RAIDEURS GENERALISES
! IN  : NBCHOC : NOMBRE DE NOEUDS DE CHOC
! IN  : PARCHO : TABLEAU DES PARAMETRES DE CHOC
! IN  : NOECHO : TABLEAU DES NOMS DES NOEUDS DE CHOC
! OUT : SEUIL  :
! ----------------------------------------------------------------------
!
!
!
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/calsvd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdtrib.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtdscr.h"
#include "asterfort/posddl.h"
#include "asterfort/preres.h"
#include "asterfort/resoud.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "blas/ddot.h"
    integer :: nbchoc, info, nbmode, irigi, indic(nbmode), ibid
    integer :: vali
    integer :: neq, nbnli, istoav, iret, ifac
    integer :: jslvi
    real(kind=8) :: riggen(*), seuil, parcho(nbnli, *)
    character(len=8) :: noecho(nbnli, *)
    real(kind=8) :: trloc(nbmode), soupl(nbmode), trlocj
    real(kind=8) :: valr(3)
    real(kind=8) :: fimpo(neq), rfimpo(neq)
    real(kind=8) :: bmodal(neq, nbmode), soup, cef, tx
    character(len=19) :: marig
    character(len=19) :: solveu, matpre
    character(len=24) :: valk
    complex(kind=8) :: cbid
    integer :: i, j, jj, k, ia, ic, jm, iddlx, iddly, iddlz, nunoe
    real(kind=8) :: cc, cs, ct, scf, rscf, usr, normx
!
    logical :: matuv
    integer :: nm, m, n, ierr, nblig, icolc
    integer :: nbch1, nbch2, neqch1, neqch2
    integer :: jefloc, jrfimp, jnormx, jnormy, ja, jw, ju, jv
    real(kind=8) :: mmax, mmin, scond, eps
    cbid = dcmplx(0.d0, 0.d0)
!
!
!      SEUIL=1.D0
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    seuil=0.d0
!      EPS=1.D-50
    eps = r8prem( )
    ifac = 0
    nblig = neq
    icolc = 0
!
    nbch1 = 1+2*nbchoc
    nbch2 = 2*nbchoc
    neqch1 = neq*nbch1
    neqch2 = neq*nbch2
!
    call mtdscr(marig)
    call jeveuo(marig//'.&INT', 'E', irigi)
!
    call wkvect('&&CRICHO.EFLOC', 'V V R', nbmode, jefloc)
    call wkvect('&&CRICHO.RFIMPOX', 'V V R', neqch2, jrfimp)
    call wkvect('&&CRICHO.NORMXX', 'V V R', nbch2, jnormx)
    call wkvect('&&CRICHO.NORMY', 'V V R', nbmode, jnormy)
    call wkvect('&&CRICHO.A', 'V V R', neqch1, ja)
    call wkvect('&&CRICHO.W', 'V V R', nbch1, jw)
    call wkvect('&&CRICHO.U', 'V V R', neqch1, ju)
    call wkvect('&&CRICHO.V', 'V V R', neqch1, jv)
!
    if (nbchoc .gt. 0) then
        do i = 1, nbchoc
            jm=1
            if (noecho(i,9)(1:2) .eq. 'BI') jm=2
            do jj = 1, jm
                icolc = icolc+1
                ct=0.d0
                cef=0.d0
                ic=4*jj-3
                call utmess('I', 'VIDE_1')
                if (info .ge. 2) then
                    valk = noecho(i,ic)
                    call utmess('I', 'SOUSTRUC_85', sk=valk)
                endif
!     CREATION DE FIMPO : FORCE UNITAIRE AU NOEUD DE CHOC (N)
                call utmess('I', 'VIDE_1')
                do k = 1, neq
                    fimpo(k)=0.d0
                end do
                call posddl('NUME_DDL', noecho(i, ic+2), noecho(i, ic), 'DX', nunoe,&
                            iddlx)
                call posddl('NUME_DDL', noecho(i, ic+2), noecho(i, ic), 'DY', nunoe,&
                            iddly)
                call posddl('NUME_DDL', noecho(i, ic+2), noecho(i, ic), 'DZ', nunoe,&
                            iddlz)
                fimpo(iddlx)=parcho(i,45)
                fimpo(iddly)=parcho(i,46)
                fimpo(iddlz)=parcho(i,47)
!
!           CALCUL DE RFIMPO : K*N
                call mrmult('ZERO', irigi, fimpo, rfimpo, 1,&
                            .true.)
!
                if (ifac .eq. 0) then
                    call dismoi('SOLVEUR', marig, 'MATR_ASSE', repk=solveu)
                    ASSERT(solveu.eq.'&&OP0074.SOLVEUR')
                    matpre='&&OP0074.BIDON'
!
!             ISTOP MIS A 2 POUR NE PAS ARRETER L'EXECUTION EN CAS
!             DE MATRICE SINGULIERE (MODES STATIQUES)
                    call jeveuo(solveu//'.SLVI', 'E', jslvi)
                    istoav=zi(jslvi-1+3)
                    zi(jslvi-1+3)=2
                    call preres(solveu, 'V', iret, matpre, marig,&
                                ibid, - 9999)
!             -- ON RETABLIT ISTOP
                    zi(jslvi-1+3)=istoav
                    if (iret .eq. 2) then
                        call utmess('A', 'SOUSTRUC_7')
                        goto 999
                    else if (iret.eq.1) then
                        call utmess('A', 'SOUSTRUC_8')
                        goto 999
                    endif
                    ifac=1
                endif
!
!           FIMPO : DEFORMEE STATIQUE (K-1*N)
                call resoud(marig, ' ', ' ', ' ', 1,&
                            ' ', ' ', ' ', fimpo, [cbid],&
                            ' ', .true., 0, iret)
!           NORMX : NORME K-1*N
                normx=ddot(neq,fimpo,1,fimpo,1)
                zr(jnormx-1+icolc)=normx
!           RFIMPOX : K-1*N (SAUVEGARDE DEFORMEE STATIQUE)
                do k = 1, neq
                    zr(jrfimp-1+k+neq*(icolc-1))=fimpo(k)
                end do
!
!     CALCUL DE SOUP : TN*K-1*N
                soup = parcho(i,45)*fimpo(iddlx)
                soup = soup + parcho(i,46)*fimpo(iddly)
                soup = soup + parcho(i,47)*fimpo(iddlz)
                do k = 1, neq
                    fimpo(k)=0.d0
                end do
                fimpo(iddlx)=parcho(i,45)
                fimpo(iddly)=parcho(i,46)
                fimpo(iddlz)=parcho(i,47)
                do j = 1, nbmode
                    if (riggen(j) .le. 0.d0) then
                        usr=0.d0
                    else
                        usr=1.d0/riggen(j)
                    endif
!     RSCF : TYNU*K*N
!     SCF : TYNU*N
                    rscf=ddot(neq,bmodal(1,j),1,rfimpo,1)
                    scf=ddot(neq,bmodal(1,j),1,fimpo,1)
                    cc=scf*rscf*usr
                    cs=scf**2*usr
                    if (info .ge. 2) then
                        soupl(j)=cs
                        if (soup .ne. 0.d0) then
                            trloc(j)=cs/soup
                        else
                            trloc(j)=0.d0
                        endif
                        indic(j)=j
                        trlocj=trloc(j)
                    else
                        if (soup .ne. 0.d0) then
                            trlocj=cs/soup
                        else
                            trlocj=0.d0
                        endif
                    endif
                    ct=ct+trlocj
                    zr(jefloc-1+j)=cc
                    cef = cef + cc
                end do
                parcho(i,48+jj-1)=ct
!            IF (CT.NE.0.D0) SEUIL=MIN(SEUIL,CT)
!
                if (info .ge. 2) then
!      ON ORDONNE SELON LES SOUPLESSES DECROISSANTES
                    call mdtrib(indic, soupl, nbmode)
                    do j = 1, nbmode
                        vali = indic(j)
                        valr (1) = trloc(indic(j))
                        valr (2) = soupl(indic(j))
                        valr (3) = zr(jefloc-1+indic(j))
                        call utmess('I', 'SOUSTRUC_93', si=vali, nr=3, valr=valr)
                    end do
                endif
                valk = noecho(i,ic)
                valr (1) = ct
                valr (2) = cef
                call utmess('I', 'SOUSTRUC_94', sk=valk, nr=2, valr=valr)
                tx = soup*parcho(i,2)*(1.d0-ct)
                valr (1) = tx
                call utmess('I', 'SOUSTRUC_95', sr=valr(1))
                seuil=max(seuil,tx)
                tx = soup*ct*parcho(i,2)
                valr (1) = tx
                call utmess('I', 'SOUSTRUC_96', sr=valr(1))
            end do
!
        end do
!
        if (info .ge. 2) then
            call utmess('I', 'VIDE_1')
            matuv = .false.
            nm = nblig
            m = neq
!
! LA MATRICE A CONTIENT LES DEFORMEES STATIQUES
! ICOLC : NB DE CHOC A CONSIDERER
            n = icolc
            do k = 1, neq
                do ia = 1, icolc
                    if (zr(jnormx-1+ia) .gt. eps) then
                        zr(ja-1+k+neq*(ia-1)) = zr(jrfimp-1+k+neq*(ia- 1))/sqrt(zr(jnormx-1+ia))
                    else
                        zr(ja-1+k+neq*(ia-1)) = 0.d0
                    endif
                end do
            end do
!
            call calsvd(nm, m, n, zr(ja), zr(jw),&
                        matuv, zr(ju), matuv, zr( jv), ierr)
            if (ierr .ne. 0) goto 999
            mmax = 0.d0
            mmin = 1.d10
            do ia = 1, n
                mmax = max(mmax,zr(jw-1+ia))
                mmin = min(mmin,zr(jw-1+ia))
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
                zr(jnormy-1+jj)=ddot(neq,bmodal(1,jj),1,bmodal(1,jj),&
                1)
            end do
!
            n = icolc+1
            do j = 1, nbmode
!
! LA MATRICE A CONTIENT LES DEFORMEES STATIQUES ET MODE
                do k = 1, neq
                    do ia = 1, icolc
                        if (zr(jnormx-1+ia) .gt. eps) then
                            zr(ja-1+k+neq*(ia-1)) = zr(&
                                                    jrfimp-1+k+neq* (ia-1))/sqrt(zr(jnormx-1+ia))
                        else
                            zr(ja-1+k+neq*(ia-1)) = 0.d0
                        endif
                    end do
                    zr(ja-1+k+neq*(icolc+1-1)) = bmodal(k,j)/sqrt(zr( jnormy-1+j))
                end do
!
                call calsvd(nm, m, n, zr(ja), zr(jw),&
                            matuv, zr(ju), matuv, zr(jv), ierr)
                if (ierr .ne. 0) goto 999
                mmax = 0.d0
                mmin = 1.d10
                do ia = 1, n
                    mmax = max(mmax,zr(jw-1+ia))
                    mmin = min(mmin,zr(jw-1+ia))
                end do
! CONDITIONNEMENT
                if (mmin .le. eps) then
                    vali = j
                    valr (1) = mmin
                    valr (2) = eps
                    call utmess('I', 'SOUSTRUC2_1', si=vali, nr=2, valr=valr)
                    mmin = eps
                endif
                zr(jefloc-1+j) = mmax/mmin
! NORMALISATION PAR RAPPORT DEF STATIQUE
                if (scond .le. eps) scond = eps
                zr(jefloc-1+j) = zr(jefloc-1+j)/scond
!
                indic(j)=j
            end do
!
!      ON ORDONNE SELON LA PARTICIPATION DECROISSANTE
            call mdtrib(indic, zr(jefloc), nbmode)
            do j = 1, nbmode
                vali = indic(j)
                valr (1) = zr(jefloc-1+indic(j))
                call utmess('I', 'SOUSTRUC2_2', si=vali, sr=valr(1))
            end do
!
            call utmess('I', 'VIDE_1')
        endif
!
!
    endif
!
999 continue
!
! --- MENAGE
!
    call jedetr('&&CRICHO.EFLOC')
    call jedetr('&&CRICHO.RFIMPOX')
    call jedetr('&&CRICHO.NORMXX')
    call jedetr('&&CRICHO.NORMY')
    call jedetr('&&CRICHO.A')
    call jedetr('&&CRICHO.W')
    call jedetr('&&CRICHO.U')
    call jedetr('&&CRICHO.V')
!
end subroutine
