subroutine vppost(vecrer, vecrei, vecrek, vecvp, nbpark, nbpari, nbparr, mxresf,&
                  nconv, nblagr, nfreqg, modes, typcon, compex, eigsol, matopa, matpsc, solveu,&
                  vecblo, veclag, flage,&
                  icom1, icom2, mpicou, mpicow, omemax, omemin, vpinf, vpmax, lcomod)

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
! -------------------------------------------------------------------------------------------------
! ROUTINE ORGANISANT LES POST-TRAITEMENTS ET NETTOYAGES GENERAUX D'OP0045.
! RQ. ON DETRUITS LES OBJETS GLOBAUX VECBLO, VECLAG ET, SUIVANT LES CAS, VECRIG, SUR BASE VOLATILE.
!     ILS SONT DETRUITS DANS SOLVEU, EIGSOL, VECBLO, VECLAG, VECRER, VECREI, VECREK ET VECVP.
! -------------------------------------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
    implicit none

#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/onerrf.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
#include "asterfort/vpcntl.h"
#include "asterfort/vplecs.h"
#include "asterfort/vpmpi.h"
#include "asterfort/vppara.h"
#include "asterfort/vpwecf.h"

!
! --- INPUT
!
    integer           , intent(in) :: nbpark, nbpari, nbparr, mxresf, nconv, nblagr
    integer           , intent(in) :: nfreqg
    character(len=8)  , intent(in) :: modes
    character(len=16) , intent(in) :: typcon, compex
    character(len=19) , intent(in) :: eigsol, matopa, matpsc, solveu
    character(len=24) , intent(in) :: vecrer, vecrei, vecrek, vecvp, vecblo, veclag
    logical           , intent(in) :: flage

!
! --- OUTPUT
! None
!
! --- INPUT/OUTPUT
!
    mpi_int           , intent(inout) :: mpicou, mpicow
    integer           , intent(inout) :: icom1, icom2
    real(kind=8)      , intent(inout) :: omemax, omemin, vpinf, vpmax
    logical           , intent(inout) :: lcomod

!
! --- VARIABLES LOCALES
!
    integer           :: nbpara
    parameter           (nbpara=27)
    integer           :: nparr, ibid, nbrss, iret, lraide, lmasse, lamor, neq, lddl, lprod
    integer           :: lmat(3), lmtpsc, lmatra, ierx
    integer           :: lresui, lresur, lresuk, lvec
    real(kind=8)      :: omecor, rbid, precdc, precsh, seuil
    complex(kind=8)   :: czero
    character(len=1)  :: k1bid, ktyp, k1blan, ctyp
    character(len=4)  :: k4blan
    character(len=8)  :: knega, k8bid
    character(len=9)  :: k9bid
    character(len=14) :: k14bid, matra, matrc
    character(len=16) :: k16bid, optiof, optiov, stoper, sturm, typres
    character(len=19) :: amor, masse, raide, k19bid
    character(len=24) :: nopara(nbpara), valk(2)
    logical           :: lc, lkr, lns, lbid

!
! --- DECLARATION DES DATAS
!
!     ------------------------------------------------------------------
    data  nopara /&
     &  'NUME_MODE'       , 'ITER_QR'         , 'ITER_BATHE'      ,&
     &  'ITER_ARNO'       , 'ITER_JACOBI'     , 'ITER_SEPARE'     ,&
     &  'ITER_AJUSTE'     , 'ITER_INVERSE'    ,&
     &  'NORME'           , 'METHODE'         , 'TYPE_MODE'       ,&
     &  'FREQ'            ,&
     &  'OMEGA2'          , 'AMOR_REDUIT'     , 'ERREUR'          ,&
     &  'MASS_GENE'       , 'RIGI_GENE'       , 'AMOR_GENE'       ,&
     &  'MASS_EFFE_DX'    , 'MASS_EFFE_DY'    , 'MASS_EFFE_DZ'    ,&
     &  'FACT_PARTICI_DX' , 'FACT_PARTICI_DY' , 'FACT_PARTICI_DZ' ,&
     &  'MASS_EFFE_UN_DX' , 'MASS_EFFE_UN_DY' , 'MASS_EFFE_UN_DZ' /
!     ------------------------------------------------------------------

! -----------------------
! --- CORPS DE LA ROUTINE
! -----------------------


! --  INITS.
    call jemarq()
    call jeveuo(vecrer, 'E', lresur)
    call jeveuo(vecrei, 'E', lresui)
    call jeveuo(vecrek, 'E', lresuk)
    call jeveuo(vecvp,  'E', lvec)
    call jeveuo(vecblo, 'L', lprod)
    call jeveuo(veclag, 'L', lddl)
    czero=dcmplx(0.d0,0.d0)
    k1blan=' '
    k4blan='    '

! --  LECTURE DES DONNEES DE EIGSOL
    call vplecs(eigsol,&
                ibid, ibid, ibid, ibid, ibid, ibid, ibid, nbrss, ibid, ibid,&
                rbid, omecor, rbid, rbid, precdc, precsh, rbid, rbid, seuil, rbid, rbid,&
                rbid,&
                k1bid, k8bid, k8bid, k9bid, matra, k14bid, matrc, k16bid, optiof, stoper, sturm,&
                k16bid, typres, amor, masse, raide, k19bid,&
                lc, lkr, lns, lbid, lbid)
    if (lkr) then
        ktyp='R'
    else
        ktyp='C'
    endif

! --  DESCRIPTEURS MATRICES
    call jeveuo(raide//'.&INT', 'E', lraide)
    neq = zi(lraide+2)
    call jeveuo(masse//'.&INT', 'E', lmasse)
    if (lc) then
        call jeveuo(amor//'.&INT', 'E', lamor)          
    else
        lamor=0
    endif
    call jeexin(matpsc//'.&INT', iret)
    if (iret.eq.0) then
        lmtpsc=0
    else
        call jeveuo(matpsc//'.&INT', 'E', lmtpsc)
    endif
    call jeexin(matopa//'.&INT', iret)
    if (iret.eq.0) then
        lmatra=0
    else
        call jeveuo(matopa//'.&INT', 'E', lmatra)
    endif

! -- CALCUL DES PARAMETRES GENERALISES
! --  CALCUL DE LA NORME D'ERREUR SUR LE MODE
! --  STOCKAGE DES VECTEURS PROPRES
! --  PARALLELISME MULTI-NIVEAUX STEP 3
    knega = 'NON'
    nparr = nbparr
    if (typcon(1:9) .eq. 'MODE_ACOU') nparr = 7

    if ((.not.lc) .and. lkr .and. (.not.lns)) then
        call vppara(modes, typcon, knega, lraide, lmasse, lamor, mxresf, neq, nconv, omecor,&
                    zi(lddl), zi(lprod), zr(lvec), [czero], nbpari, nparr, nbpark, nopara,&
                    k4blan, zi(lresui), zr(lresur), zk24(lresuk), ktyp, lcomod, icom1,&
                    icom2, typres, nfreqg)
    else
        if (lcomod) ASSERT(.false.)
        call vppara(modes, typcon, knega, lraide, lmasse, lamor, mxresf, neq, nconv, omecor,&
                    zi(lddl), zi(lprod), [rbid], zc(lvec), nbpari, nparr, nbpark, nopara,&
                    k4blan, zi(lresui), zr(lresur), zk24(lresuk), ktyp, lcomod, ibid,&
                    ibid, k16bid, ibid)
    endif


! --  IMPRESSIONS LIEES A LA METHODE
    call vpwecf(k1blan, typres, nconv, mxresf, zi(lresui), zr(lresur), zk24(lresuk), lamor, ktyp,&
                lns)
    call titre()

! --  CONTROLE DE VALIDITE DES MODES CALCULES
    if (sturm(1:3) .eq. 'NON') then
        optiov = ' '
    else
        optiov = optiof
        if (lc .or. (.not.lkr) .or. lns) then
! --  POUR DEBRANCHER LE TEST DE STURM DANS VPCNTL
            optiov = ' '
            valk(1) = matra
            valk(2) = matrc
            call utmess('I', 'ALGELINE2_73', nk=2, valk=valk)
        endif
    endif

    lmat(1) = lraide
    lmat(2) = lmasse
    lmat(3) = lmtpsc
! --  SI ON MANIPULE DEUX MATRICES DYNAMIQUES (MATOPA/MATPSC), ON SE DEBARASSE DE CELLE INUTILE
!     (MATRICE + FACTORISEE EVENTUELLE) ET DE SON EVENTUELLE OCCURENCE EXTERNE (MUMPS)
    if ((lmtpsc.ne.lmatra) .and. (lmatra.ne.0)) call detrsd('MATR_ASSE', matopa)

! --  PARALLELISME MULTI-NIVEAUX STEP 4
    call vpmpi(4, k19bid,&
               ibid, ibid, lcomod, mpicou, mpicow, ibid, ibid, ibid, omemax, omemin,&
               vpinf, vpmax)

! --  ON PASSE DANS LE MODE "VALIDATION DU CONCEPT EN CAS D'ERREUR"
    call onerrf('EXCEPTION+VALID', k16bid, ibid)
    if (stoper(1:3).eq.'OUI') then
        ctyp = 'E'
    else
        ctyp = 'A'
    endif
    call vpcntl(ctyp, modes, optiov, omemin, omemax, seuil, nconv, zi(lresui), lmat, omecor,&
                precdc, ierx, vpinf, vpmax, zr(lresur), zr(lresur+3*mxresf), zr(lresur+mxresf),&
                typres, nblagr, solveu, nbrss, precsh)

    if ((stoper(1:3) .eq. 'OUI') .and. (ierx .ne. 0)) call utmess('Z', 'ALGELINE2_74',num_except=33)

    if (flage) call utmess('F', 'ALGELINE5_75')


! --  ON REMET LE MECANISME D'EXCEPTION A SA VALEUR INITIALE
    call onerrf(compex, k16bid, ibid)

! --  DESTRUCTION DE LA MATRICE DYNAMIQUE RESTANTE (VRAI MATPSC DISSOSSIEE DE MATOPA OU
! --  MATPSC POINTANT SUR MATOPA D'OU LA RECONSTRUCTION DE NOM CI-DESSOUS
    if (lmtpsc .ne. 0) then
        k19bid=zk24(zi(lmtpsc+1))(1:19)
        call detrsd('MATR_ASSE', k19bid)
    endif

! -- NETTOYAGE DES OBJETS JEVEUX GLOBAUX DE L'OPERATEUR DE LA BASE VOLATILE
    call jedetr(solveu)
    call jedetr(eigsol)
    call jedetr(vecblo)
    call jedetr(veclag)
    call jedetr(vecrer)
    call jedetr(vecrei)
    call jedetr(vecrek)
    call jedetr(vecvp)
    call jedema()
!
!     FIN DE VPPOST
!
end subroutine
