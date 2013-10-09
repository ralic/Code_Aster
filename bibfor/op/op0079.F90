subroutine op0079()
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
!  CALCUL PROJECTION SD_RESULTAT SUR BASE DE RITZ
!
!----------------------------------------------------------------------
!
    implicit none
!
!
!
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterfort/copmod.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdall2.h"
#include "asterfort/rrlds.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/trlds.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/zerlag.h"
#include "blas/dcopy.h"
#include "blas/ddot.h"
!
    integer :: jsmde, nbmode, nbo, ii, iret, nbsym, idbase
    integer :: idvec1, idvec2
!-----------------------------------------------------------------------
    integer :: iadvec, iamatr, ibid, icod, iadref
    integer :: iddeeq, idvect, iliord, imod, ind, iord, isym
    integer :: jmod, jrefa, llnequ, n0, n1, n2, n4
    integer :: nbid, neq, tmod(1)
    real(kind=8) :: bid, ebid, pij
!-----------------------------------------------------------------------
    parameter    (nbsym=3)
    character(len=1) :: typvec
    character(len=8) :: nomres, basemo, nomtyp, k8bid, res, numgen
    character(len=9) :: nosyin(nbsym)
    character(len=4) :: nosyou(nbsym), nosy
    character(len=14) :: nu, numdd1, numdd2
    character(len=16) :: typres, nomcom, typbas, matri2
    character(len=19) :: nochno
    character(len=24) :: matric, deeq
    complex(kind=8) :: cbid
    data  nosyin / 'DEPL','VITE','ACCE'/
    data  nosyou / 'DEPL','VITE','ACCE'/
!
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!
! REMARQUE : ACTUELLEMENT, SEULS LES CHAMPS DEPL, VITE ET ACCE SONT
! TRAITES. IL CONVIENDRAIT NORMALEMENT DE TRAITER LES FORC_NODAL, MAIS
! POUR CELA, IL FAUT CREER UN CHAMP EQUIVALENT DANS LA SD_TRAN_GENE.
! OBJECTIF : POUVOIR DEFINIR UN CHARGEMENT GENERALISE POUR DTM AVEC UN
! TYPAGE CORRECT POUR LE CHARGEMENT. ACTUELLEMENT, LE CHARGEMENT
! APPLIQUE AVEC EXCIT_RESU EST UNE SD RESULTAT AVEC DES CHAMPS DEPL,
! DANS DYBNA_TRAN_MODAL ET DYNA_LINE_TRAN.
!
    call jemarq()
    call infmaj()
!
! --- RECUPERATION DES ARGUMENTS DE LA COMMANDE
!
    call getres(nomres, typres, nomcom)
    call getvid(' ', 'NUME_DDL_GENE', scal=numgen, nbret=n0)
    call getvid(' ', 'RESU', scal=res, nbret=n1)
! LE CAS RESU_GENE N'EST PAS ACTIVE POUR LE MOMENT
!      CALL GETVID(' ','RESU_GENE',0,IARG,1,RES,N3)
    call getvid(' ', 'BASE', scal=basemo, nbret=n4)
    call getvtx(' ', 'TYPE_VECT', scal=nomtyp, nbret=n2)
    call gettco(basemo, typbas)
!
! --- RECUPERATION DU NB DE MODES
!
    call rsorac(basemo, 'LONUTI', ibid, bid, k8bid,&
                cbid, ebid, 'ABSOLU', tmod, 1,&
                nbid)
    nbmode=tmod(1)
!
!
    call jeveuo(numgen//'      .SMOS.SMDE', 'L', jsmde)
!
!
! --- RECUPERATION DU NOMBRE DE NUME_ORDRE DE LA SD_RESU
!
    call rsorac(res, 'LONUTI', ibid, bid, k8bid,&
                cbid, ebid, 'ABSOLU', tmod, 1,&
                nbid)
    nbo=tmod(1)
    call jeveuo(res//'           .ORDR', 'L', iliord)
!
!
! --- VERIFICATION DE LA CONFORMITE DES NUMEROTATIONS
!     DES MODES ET DU VECTEUR ASSEMBLE
!     ON RECUPERE LES NUME_DDL DANS LES REFD DES DEUX SD
!     SI ELLES SONT ABSENTES, ON ESSAYE AVEC LES MATRICES
!
!
!
!
    if (typbas(1:9) .eq. 'MODE_MECA') then
        call dismoi('NUME_DDL', res, 'RESU_DYNA', repk=nu)
        if (nu(1:1) .ne. ' ') then
            numdd1=nu
        else
            call dismoi('REF_RIGI_PREM', res, 'RESU_DYNA', repk=matric)
            call exisd('MATR_ASSE', matric, iret)
            if (iret .ne. 0) then
                call dismoi('NOM_NUME_DDL', matric, 'MATR_ASSE', repk=nu)
                numdd1=nu
            endif
            if (iret .eq. 0) then
                call utmess('F', 'ALGORITH17_8', sk=res)
            endif
        endif
        call dismoi('NUME_DDL', basemo, 'RESU_DYNA', repk=nu)
        if (nu(1:1) .ne. ' ') then
            numdd2=nu
        else
            call dismoi('REF_RIGI_PREM', basemo, 'RESU_DYNA', repk=matric)
            call exisd('MATR_ASSE', matric, iret)
            if (iret .ne. 0) then
                call dismoi('NOM_NUME_DDL', matric, 'MATR_ASSE', repk=nu)
                numdd2=nu
            endif
            if (iret .eq. 0) then
                call utmess('F', 'ALGORITH17_8', sk=basemo)
            endif
        endif
!
    else if (typbas(1:9).eq.'MODE_GENE') then
        call dismoi('NUME_DDL', res, 'RESU_DYNA', repk=nu)
        call dismoi('REF_RIGI_PREM', basemo, 'RESU_DYNA', repk=matric)
        matri2 = matric(1:16)
        call jeveuo(matri2//'   .REFA', 'L', jrefa)
        numdd2=zk24(jrefa-1+2)
    endif
!
    if (numdd1 .ne. numdd2) then
        call utmess('I', 'ALGORITH9_41')
    endif
!
! --- RECUPERATION DU NOMBRE D'EQUATIONS DU SYSTEME PHYSIQUE
!
    if ((typbas(1:9).eq.'MODE_MECA')) then
        call dismoi('NB_EQUA', numdd1, 'NUME_DDL', repi=neq)
    else if (typbas(1:9).eq.'MODE_GENE') then
        call jeveuo(numdd1//'.NUME.NEQU', 'L', llnequ)
        neq = zi(llnequ)
    endif
!
    deeq = nu//'.NUME.DEEQ'
    call jeveuo(deeq, 'L', iddeeq)
!
! --- INITIALISATION DE LA SD_RESULTAT
!
    call mdall2(nomres, basemo, res, nbo, nbmode)
!
! --- RECUPERE LA BASE MODALE SOUS LA FORME D'UN VECT NBMODE*NEQ
!
    call wkvect('&&OP0072.BASEMO', 'V V R', nbmode*neq, idbase)
    call copmod(basemo, 'DEPL', neq, nu, nbmode,&
                'R', zr(idbase), [cbid])
!
! --- BOUCLE SUR LES NUM_ORDR ET LES NOMSY DE LA SD_RESULTAT
!     ATTENTION : ON NE TRAITE QUE LES NOMSY STOCKABLES DANS
!     UN TRAN_GENE : DEPL, ACCE, VITE
!
    do isym = 1, nbsym
!
        do iord = 1, nbo
!
            nosy=nosyou(isym)
!
! --- RECUP DU CHAMP DE LA SDIN CORRESPONDANT AU NUME_ORDR ET ISYM
            call rsexch(' ', res, nosyin(isym), zi(iliord-1+iord), nochno,&
                        iret)
            if (iret .ne. 0) goto 40
            call jeveuo(nochno//'.VALE', 'L', iadvec)
            call jeveuo(nochno//'.REFE', 'L', iadref)
            call jelira(nochno//'.VALE', 'TYPE', cval=typvec)
! --- LE CAS COMPLEXE (SD HARMONIQUES) N'EST PAS TRAITE
            if (typvec .eq. 'C') then
                call utmess('F', 'ALGORITH17_19')
            endif
!
! --- INDICE DE STOCKAGE
            call jeveuo(nomres//'           .'//nosy, 'E', ii)
!
            if (nomtyp(1:4) .eq. 'FORC') then
!
! --- PROJECTION D UN VECTEUR DE TYPE FORCE
!
                call wkvect('&&OP0079.VECTASSE', 'V V R', neq, idvect)
                do imod = 1, nbmode
!
!
! --------- RECOPIE DU IEME MODE DANS UN VECTEUR TEMP
!
                    call dcopy(neq, zr(idbase+(imod-1)*neq), 1, zr( idvect), 1)
!
! ------- MISE A ZERO DES DDLS DE LAGRANGE
!
                    call zerlag(neq, zi(iddeeq), vectr=zr(idvect))
!
! ------- PRODUIT SCALAIRE VECTASS * MODE
!
                    ind = ii-1+(iord-1)*nbmode+imod
                    zr(ind) = ddot(neq,zr(idvect),1,zr(iadvec),1)
!
! ------- LIBERATION DU VECTEUR TEMP
                end do
                call jedetr('&&OP0079.VECTASSE')
            else
!
! --- PROJECTION D UN VECTEUR DE TYPE DEPL OU VITE
!
                call wkvect('&&OP0079.VECTASS1', 'V V R', neq, idvec1)
                call wkvect('&&OP0079.VECTASS2', 'V V R', neq, idvec2)
                call wkvect('&&OP0079.MATRNORM', 'V V R', nbmode* nbmode, iamatr)
!
! ----- CALCUL DE TMODE*MODE
!
                do imod = 1, nbmode
!
! ----- RECOPIE DU IEME MODE
!
                    call dcopy(neq, zr(idbase+(imod-1)*neq), 1, zr( idvec1), 1)
!
! ------- MISE A ZERO DES DDLS DE LAGRANGE
!
                    call zerlag(neq, zi(iddeeq), vectr=zr(idvec1))
!
!-------- PRODUIT SCALAIRE MODE(IMOD)*MODE(JMOD)
!
                    do jmod = imod, nbmode
!
! ------- RECOPIE DU JEME MODE
!
                        call dcopy(neq, zr(idbase+(jmod-1)*neq), 1, zr( idvec2), 1)
! --------- MISE A ZERO DES DDLS DE LAGRANGE
!
                        call zerlag(neq, zi(iddeeq), vectr=zr(idvec2))
!
! --------- PRODUIT SCALAIRE MODE(IMOD)*MODE(JMOD)
!
                        pij = ddot(neq,zr(idvec1),1,zr(idvec2),1)
                        zr(iamatr+imod+ (jmod-1)*nbmode-1) = pij
                        zr(iamatr+jmod+ (imod-1)*nbmode-1) = pij
                    end do
                end do
!
! ----- CALCUL DE LA PROJECTION
!
                do imod = 1, nbmode
!
! ------- RECOPIE DU IEME MODE
!
                    call dcopy(neq, zr(idbase+(imod-1)*neq), 1, zr( idvec1), 1)
!
! ------- MISE A ZERO DES DDLS DE LAGRANGE
!
                    call zerlag(neq, zi(iddeeq), vectr=zr(idvec1))
!
! ------- PRODUIT SCALAIRE VECTASS * MODE
!
                    zr(idvec2+imod-1) = ddot(neq,zr(idvec1),1,zr( iadvec),1)
!
                end do
!
! ----- FACTORISATION ET RESOLUTION SYSTEME
!
                ind = ii-1+(iord-1)*nbmode+imod
                call trlds(zr(iamatr), nbmode, nbmode, icod)
                if (icod .ne. 0) then
                    call utmess('F', 'ALGORITH9_42')
                endif
                call rrlds(zr(iamatr), nbmode, nbmode, zr(idvec2), 1)
                call dcopy(nbmode, zr(idvec2), 1, zr(ind), 1)
                call jedetr('&&OP0079.VECTASS1')
                call jedetr('&&OP0079.VECTASS2')
                call jedetr('&&OP0079.MATRNORM')
                if (typvec .eq. 'C') call jedetr('&&OP0079.VECTASC2')
            endif
!
        end do
 40     continue
    end do
!
    call jedema()
end subroutine
