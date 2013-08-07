subroutine op0072()
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!
!  CALCUL PROJECTION VECTEUR SUR BASE DE RITZ
!
!-----------------------------------------------------------------------
!
    implicit none
!
!
!
!
#include "jeveux.h"
!
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterc/getvid.h"
#include "asterc/getvtx.h"
#include "asterfort/copmod.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rclds.h"
#include "asterfort/rrlds.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/trlds.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
#include "asterfort/zerlag.h"
#include "blas/dcopy.h"
#include "blas/ddot.h"
#include "blas/zcopy.h"
#include "blas/zdotc.h"
    integer :: jsmde
    character(len=1) :: typvec
    character(len=8) :: nomres, basemo, vectas, nomtyp, maill1, maill2, k8bid
    character(len=14) :: numgen, numdd1, numdd2
    character(len=16) :: typres, nomcom, typbas, matri2
    character(len=19) :: proch1, proch2, nomcha
    character(len=24) :: matric, kbid, deeq, typeba, valk(24)
    complex(kind=8) :: cbid, dcmplx
    real(kind=8) :: zero
    integer :: iarg
!
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iadesc, iadref, iadrif, iadvec, iamatr, iarefe
    integer :: iavale, ibid, icod, idbase, iddeeq, idvec1, idvec2
    integer :: idvec3, idvec4, idvect, iret, j, jrefa, llnequ
    integer :: n0, n1, n2, n3, n4, nbid, nbmode
    integer :: neq
    real(kind=8) :: bid, ebid, pij
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
    zero = 0.d0
!
! --- RECUPERATION DES ARGUMENTS DE LA COMMANDE
!
    call getres(nomres, typres, nomcom)
    call getvid(' ', 'NUME_DDL_GENE', 0, iarg, 1,&
                numgen, n0)
    call getvid(' ', 'VECT_ASSE', 0, iarg, 1,&
                vectas, n1)
    call getvid(' ', 'VECT_ASSE_GENE', 0, iarg, 1,&
                vectas, n3)
    call getvid(' ', 'BASE', 0, iarg, 1,&
                basemo, n4)
    call getvtx(' ', 'TYPE_VECT', 0, iarg, 1,&
                nomtyp, n2)
    call gettco(basemo, typbas)
!
! --- RECUPERATION DU NB DE MODES
!
    call rsorac(basemo, 'LONUTI', ibid, bid, k8bid,&
                cbid, ebid, 'ABSOLU', nbmode, 1,&
                nbid)
!
!
    call jeveuo(numgen//'.SMOS.SMDE', 'L', jsmde)
!
! --- VERIFICATION DE LA CONFORMITE DES NUMEROTATIONS
!     DES MODES ET DU VECTEUR ASSEMBLE
!
    call jeveuo(vectas//'           .VALE', 'L', iadvec)
    call jeveuo(vectas//'           .REFE', 'L', iadref)
    call jeveuo(basemo//'           .REFD', 'L', iadrif)
    call jelira(vectas//'           .VALE', 'TYPE', cval=typvec)
    typeba = zk24(iadrif+6)
!
    if (typbas(1:9) .eq. 'MODE_MECA') then
        proch1 = zk24(iadref+1)
        matric = zk24(iadrif)
        if (typeba(1:1) .eq. ' ') then
            call exisd('MATR_ASSE', matric, iret)
            if (iret .ne. 0) then
                call dismoi('F', 'PROF_CHNO', matric, 'MATR_ASSE', ibid,&
                            proch2, iret)
            else
                proch2 = proch1
            endif
        else
            proch2 = zk24(iadrif+3)(1:14)//'.NUME'
        endif
!
        if (proch1 .ne. proch2) then
            call u2mess('I', 'ALGORITH9_41')
        endif
!
    else if (typbas(1:9).eq.'MODE_GENE') then
        numdd1=zk24(iadref+1)
        proch1 = numdd1//'.NUME'
        matric = zk24(iadrif)
        matri2 = matric(1:16)
        call jeveuo(matri2//'   .REFA', 'L', jrefa)
        numdd2=zk24(jrefa-1+2)
        if (numdd1 .ne. numdd2) then
            call u2mess('I', 'ALGORITH9_41')
        endif
    endif
!
! --- RECUPERATION DU NOMBRE D'EQUATIONS DU SYSTEME PHYSIQUE
!
!
    if ((typbas(1:9).eq.'MODE_MECA')) then
        call dismoi('F', 'NB_EQUA', vectas, 'CHAM_NO', neq,&
                    kbid, iret)
        deeq = proch1//'.DEEQ'
    else if (typbas(1:9).eq.'MODE_GENE') then
        call jeveuo(numdd1//'.NUME.NEQU', 'L', llnequ)
        neq = zi(llnequ)
        deeq = numdd1//'.NUME.DEEQ'
    endif
!
    call jeveuo(deeq, 'L', iddeeq)
!
! --- CREATION DE L OBJET VECT_GENE RESULTAT
!
    if (typvec .eq. 'R') then
        call wkvect(nomres//'           .VALE', 'G V R', nbmode, iavale)
    else
        call wkvect(nomres//'           .VALE', 'G V C', nbmode, iavale)
        call wkvect('&&OP0072.VECTASC1', 'V V C', neq, idvec3)
    endif
    call wkvect(nomres//'           .REFE', 'G V K24', 2, iarefe)
    call wkvect(nomres//'           .DESC', 'G V I', 3, iadesc)
    call jeecra(nomres//'           .DESC', 'DOCU', cval='VGEN')
!
! --- REMPLISSAGE DU .REFE ET .VALE
!
    zk24(iarefe) = basemo
    zk24(iarefe+1) = numgen//'.NUME     '
    zi(iadesc) = 1
    zi(iadesc+1) = nbmode
!
!   LE STOCKAGE EST-IL DIAGONAL ?
    if (zi(jsmde-1+4) .eq. zi(jsmde-1+1)) then
        zi(iadesc+2) = 1
    else
        zi(iadesc+2) = 2
    endif
    call wkvect('&&OP0072.BASEMO', 'V V R', nbmode*neq, idbase)
!
! --- VERIFICTION QUE LES MAILLAGES DU CHAMP A PROJETER ET DES CHAMPS
!   - DE LA BASE MODALE SONT IDENTIQUES
!   - 1. MAILLAGE DE REFERENCE POUR LA BASE
    call rsexch('F', basemo, 'DEPL', 1, nomcha,&
                iret)
    call dismoi('F', 'NOM_MAILLA', nomcha, 'CHAM_NO', ibid,&
                maill1, iret)
!   - 2. MAILLAGE DE REFERENCE POUR LE CHAM_NO
    call dismoi('F', 'NOM_MAILLA', vectas, 'CHAM_NO', ibid,&
                maill2, iret)
    if (maill1 .ne. maill2) then
        valk (1) = proch2
        valk (2) = maill2
        valk (3) = proch1
        valk (4) = maill1
        call u2mesg('F', 'ALGORITH12_62', 4, valk, 0,&
                    0, 0, 0.d0)
    endif
!
! --- CONVERSION DE BASEMO A LA NUMEROTATION NU
    call copmod(basemo, 'DEPL', neq, proch1, nbmode,&
                'R', zr(idbase), cbid)
!
    if (nomtyp(1:4) .eq. 'FORC') then
!
! --- PROJECTION D UN VECTEUR DE TYPE FORCE
!
        call wkvect('&&OP0072.VECTASSE', 'V V R', neq, idvect)
        do 10 i = 1, nbmode
!
! --------- RECOPIE DU IEME MODE
!
            call dcopy(neq, zr(idbase+(i-1)*neq), 1, zr(idvect), 1)
!
!
! ------- MISE A ZERO DES DDLS DE LAGRANGE
!
            call zerlag('R', zr(idvect), cbid, neq, zi(iddeeq))
!
! ------- PRODUIT SCALAIRE VECTASS * MODE
!
            if (typvec .eq. 'R') then
                zr(iavale+i-1) = ddot(neq,zr(idvect),1,zr(iadvec),1)
            else
                do 666 j = 1, neq
                    zc(idvec3+j-1)=dcmplx(zr(idvect+j-1),zero)
666              continue
                zc(iavale+i-1) = zdotc(neq,zc(idvec3),1,zc(iadvec),1)
            endif
10      continue
    else
!
! --- PROJECTION D UN VECTEUR DE TYPE DEPL OU VITE
!
        call wkvect('&&OP0072.VECTASS1', 'V V R', neq, idvec1)
        call wkvect('&&OP0072.VECTASS2', 'V V R', neq, idvec2)
        if (typvec .eq. 'C') then
            call wkvect('&&OP0072.VECTASC2', 'V V R', neq, idvec4)
        endif
        call wkvect('&&OP0072.MATRNORM', 'V V R', nbmode*nbmode, iamatr)
!
! ----- CALCUL DE TMODE*MODE
!
        do 20 i = 1, nbmode
!
! ----- RECOPIE DU IEME MODE
!
            call dcopy(neq, zr(idbase+(i-1)*neq), 1, zr(idvec1), 1)
!
! ------- MISE A ZERO DES DDLS DE LAGRANGE
!
            call zerlag('R', zr(idvec1), cbid, neq, zi(iddeeq))
!
!-------- PRODUIT SCALAIRE MODE(I)*MODE(J)
!
            do 20 j = i, nbmode
!
! ------- RECOPIE DU JEME MODE
!
                call dcopy(neq, zr(idbase+(j-1)*neq), 1, zr(idvec2), 1)
! --------- MISE A ZERO DES DDLS DE LAGRANGE
!
                call zerlag('R', zr(idvec2), cbid, neq, zi(iddeeq))
!
! --------- PRODUIT SCALAIRE MODE(I)*MODE(J)
!
                pij = ddot(neq,zr(idvec1),1,zr(idvec2),1)
                zr(iamatr+i+ (j-1)*nbmode-1) = pij
                zr(iamatr+j+ (i-1)*nbmode-1) = pij
20          continue
!
! ----- CALCUL DE LA PROJECTION
!
        do 30 i = 1, nbmode
!
! ------- RECOPIE DU IEME MODE
!
            call dcopy(neq, zr(idbase+(i-1)*neq), 1, zr(idvec1), 1)
!
! ------- MISE A ZERO DES DDLS DE LAGRANGE
!
            call zerlag('R', zr(idvec1), cbid, neq, zi(iddeeq))
!
! ------- PRODUIT SCALAIRE VECTASS * MODE
!
            if (typvec .eq. 'R') then
                zr(idvec2+i-1) = ddot(neq,zr(idvec1),1,zr(iadvec),1)
            else
                do 667 j = 1, neq
                    zc(idvec3+j-1)=dcmplx(zr(idvec1+j-1),zero)
667              continue
                zc(idvec4+i-1) = zdotc(neq,zc(idvec3),1,zc(iadvec),1)
            endif
30      continue
!
! ----- FACTORISATION ET RESOLUTION SYSTEME
!
        call trlds(zr(iamatr), nbmode, nbmode, icod)
        if (icod .ne. 0) then
            call u2mess('F', 'ALGORITH9_42')
        endif
        if (typvec .eq. 'R') then
            call rrlds(zr(iamatr), nbmode, nbmode, zr(idvec2), 1)
            call dcopy(nbmode, zr(idvec2), 1, zr(iavale), 1)
        else
            call rclds(zr(iamatr), nbmode, nbmode, zc(idvec4), 1)
            call zcopy(nbmode, zc(idvec3), 1, zc(iavale), 1)
        endif
    endif
!
    call jedema()
end subroutine
