subroutine mstget(nomcmp, matric, motfac, nbind, ddlsta)
    implicit none
#include "jeveux.h"
#include "asterfort/compno.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvem.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/noeddl.h"
#include "asterfort/pteddl.h"
#include "asterfort/rgndas.h"
#include "asterfort/typddl.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nbind, ddlsta(*)
    character(len=*) :: nomcmp, matric, motfac
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     OPERATEUR : MODE_STATIQUE
!     RECUPERATION DES DDL SUR LESQUELS IL FAUT CALCULER DES MODES STATS
!     ------------------------------------------------------------------
! IN  : NOMCMP : NOM DE LA COMMANDE
! IN  : MATRIC : NOM DE LA MATRICE ASSEMBLEE DU SYSTEME
! IN  : MOTFAC : MOT FACTEUR  'MODE_STAT', 'FORCE_NODALE', 'PSEUDO_MODE'
! IN  : NBIND  : NOMBRE DE MOT CLE FACTEUR
! OUT : DDLSTA : TABLEAU DES DDL
!                DDLSTA(I) = 0  PAS DE MODE STATIQUE POUR LE DDL I
!                DDLSTA(I) = 1  MODE STATIQUE POUR LE DDL I
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: neq
    character(len=8) :: nomma, nomnoe, kbid
    character(len=14) :: nume
    character(len=24) :: manono, magrno, texte, text1, text2, text3, nomgr
    character(len=24) :: valk(4)
    integer :: iarg
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ic, idgn, ieq, ii
    integer :: iii, imode, in, ind, ing, iret, jcmp
    integer :: jind1, jind2, jnoe, lacb, lact, lblo, lcmp
    integer :: ldgn, llag, lnoe, na, nac, nb, nba
    integer :: nbb, nbgr, nbl, nbliai, ncmp, nd, ni
    integer :: nnoe, nsc, nt, ntc
!-----------------------------------------------------------------------
    call jemarq()
    magrno = ' '
    manono = ' '
    text1 = 'UN DDL N EST PAS BLOQUE '
    text2 = 'UN DDL N EST PAS LIBRE  '
    text3 = 'UN DDL EST UN LAGRANGE  '
!
    call dismoi('NOM_MAILLA', matric, 'MATR_ASSE', repk=nomma)
    call dismoi('NOM_NUME_DDL', matric, 'MATR_ASSE', repk=nume)
    call dismoi('NB_EQUA', matric, 'MATR_ASSE', repi=neq)
    magrno = nomma//'.GROUPENO'
    manono = nomma//'.NOMNOE'
    call wkvect('&&MSTGET.LISTE.LAGRAN', 'V V I', neq, llag)
    call wkvect('&&MSTGET.LISTE.BLOQUE', 'V V I', neq, lblo)
    call wkvect('&&MSTGET.LISTE.ACTIF', 'V V I', neq, lact)
    call wkvect('&&MSTGET.LISTE.ACTBLO', 'V V I', neq, lacb)
    call typddl('LAGR', nume, neq, zi(llag), nba,&
                nbb, nbl, nbliai)
    call typddl('BLOQ', nume, neq, zi(lblo), nba,&
                nbb, nbl, nbliai)
    call typddl('ACTI', nume, neq, zi(lact), nba,&
                nbb, nbl, nbliai)
    call typddl('ACBL', nume, neq, zi(lacb), nba,&
                nbb, nbl, nbliai)
!
    if (motfac(1:9) .eq. 'MODE_STAT') then
        jind1 = lblo
        jind2 = lact
        texte = text1
    else if (motfac(1:12).eq.'FORCE_NODALE') then
        jind1 = lact
        jind2 = lblo
        texte = text2
    else if (motfac(1:11).eq.'PSEUDO_MODE') then
        jind1 = lacb
        jind2 = llag
        texte = text3
    else if (motfac(1:11).eq.'MODE_INTERF') then
        jind1 = lblo
        jind2 = lact
        texte = text1
    else
        call utmess('F', 'ALGELINE2_5')
    endif
!
    do i = 1, nbind
        if (motfac(1:11) .eq. 'PSEUDO_MODE') then
            call getvtx(motfac, 'AXE', iocc=i, nbval=0, nbret=na)
            call getvtx(motfac, 'DIRECTION', iocc=i, nbval=0, nbret=nd)
            if ((na+nd) .ne. 0) goto 10
        endif
!
!
!        --- LES NOEUDS ---
        call getvtx(motfac, 'TOUT', iocc=i, nbval=0, nbret=nt)
        if (nt .ne. 0) then
            lnoe = jind1
        endif
!
        call getvem(nomma, 'NOEUD', motfac, 'NOEUD', i,&
                    iarg, 0, kbid, nnoe)
        if (nnoe .ne. 0) then
            nnoe = -nnoe
            call wkvect('&&MSTGET.NOM.NOEUD', 'V V K8', nnoe, jnoe)
            call getvem(nomma, 'NOEUD', motfac, 'NOEUD', i,&
                        iarg, nnoe, zk8( jnoe), ni)
            call wkvect('&&MSTGET.LISTE.NOEUD', 'V V I', neq, lnoe)
            call noeddl(nume, nnoe, zk8(jnoe), neq, zi(lnoe))
        endif
!
        call getvem(nomma, 'GROUP_NO', motfac, 'GROUP_NO', i,&
                    iarg, 0, kbid, nbgr)
        if (nbgr .ne. 0) then
            nbgr = -nbgr
            call wkvect('&&MSTGET.GROUP_NO', 'V V K24', nbgr, idgn)
            call getvem(nomma, 'GROUP_NO', motfac, 'GROUP_NO', i,&
                        iarg, nbgr, zk24(idgn), ni)
!           --- ECLATE LE GROUP_NO EN NOEUD ---
            call compno(nomma, nbgr, zk24(idgn), nnoe)
            call wkvect('&&MSTGET.POSITION.NOEUD', 'V V K8', nnoe, jnoe)
            ii = -1
            do ing = 1, nbgr
                nomgr = zk24(idgn+ing-1)
                call jelira(jexnom(magrno, nomgr), 'LONUTI', nb)
                call jeveuo(jexnom(magrno, nomgr), 'L', ldgn)
                do in = 0, nb-1
                    call jenuno(jexnum(manono, zi(ldgn+in)), nomnoe)
                    ii = ii + 1
                    zk8(jnoe+ii) = nomnoe
                end do
            end do
            call wkvect('&&MSTGET.LISTE.NOEUD', 'V V I', neq, lnoe)
            call noeddl(nume, nnoe, zk8(jnoe), neq, zi(lnoe))
        endif
!
!        --- LES COMPOSANTES ---
        call getvtx(motfac, 'TOUT_CMP', iocc=i, nbval=0, nbret=ntc)
        if (ntc .ne. 0) then
            call wkvect('&&MSTGET.LISTE.CMP', 'V V I', neq, lcmp)
            do ieq = 0, neq-1
                zi(lcmp+ieq) = 1
            end do
        endif
!
        call getvtx(motfac, 'AVEC_CMP', iocc=i, nbval=0, nbret=nac)
        if (nac .ne. 0) then
            ncmp = -nac
            call wkvect('&&MSTGET.NOM.CMP', 'V V K8', ncmp, jcmp)
            call getvtx(motfac, 'AVEC_CMP', iocc=i, nbval=ncmp, vect=zk8(jcmp),&
                        nbret=ni)
            call wkvect('&&MSTGET.LISTE.CMP', 'V V I', neq*ncmp, lcmp)
            call pteddl('NUME_DDL', nume, ncmp, zk8(jcmp), neq,&
                        zi(lcmp))
            do ic = 2, ncmp
                ind = (ic-1)*neq
                do ieq = 0, neq-1
                    zi(lcmp+ieq)= max(zi(lcmp+ind+ieq),zi(lcmp+ieq))
                end do
            end do
        endif
!
        call getvtx(motfac, 'SANS_CMP', iocc=i, nbval=0, nbret=nsc)
        if (nsc .ne. 0) then
            ncmp = -nsc
            call wkvect('&&MSTGET.NOM.CMP', 'V V K8', ncmp, jcmp)
            call getvtx(motfac, 'SANS_CMP', iocc=i, nbval=ncmp, vect=zk8(jcmp),&
                        nbret=ni)
            ncmp = ncmp + 1
            zk8(jcmp+ncmp-1) = 'LAGR'
            call wkvect('&&MSTGET.LISTE.CMP', 'V V I', neq*ncmp, lcmp)
            call pteddl('NUME_DDL', nume, ncmp, zk8(jcmp), neq,&
                        zi(lcmp))
            do ic = 2, ncmp
                ind = (ic-1)*neq
                do ieq = 0, neq-1
                    zi(lcmp+ieq)= max(zi(lcmp+ind+ieq),zi(lcmp+ieq))
                end do
            end do
            do ieq = 0, neq-1
                zi(lcmp+ieq)= 1 - zi(lcmp+ieq)
            end do
        endif
!
!        --- ON VERIFIE :
!               POUR DDL_IMPO TOUS LES DDL DONNES SONT BLOQUES
!               POUR FORCE_NODALE TOUS LES DDL DONNES SONT LIBRES
!
        do ieq = 0, neq-1
            ii = ieq + 1
            imode = zi(lnoe+ieq) * zi(lcmp+ieq)
            iii = zi(jind2+ieq) * imode
            if (iii .ne. 0) then
                call rgndas(nume, ii, nomnoe, nomcmp, kbid,&
                            kbid, kbid)
                valk (1) = texte
                valk (2) = motfac
                valk (3) = nomnoe
                valk (4) = nomcmp
                call utmess('E', 'ALGELINE4_24', nk=4, valk=valk)
                imode = 0
            endif
            ddlsta(ii)= max(ddlsta(ii),imode)
        end do
!
!        --- NETTOYAGE ---
!
        call jeexin('&&MSTGET.LISTE.NOEUD', iret)
        if (iret .gt. 0) call jedetr('&&MSTGET.LISTE.NOEUD')
        call jedetr('&&MSTGET.LISTE.CMP')
        call jeexin('&&MSTGET.NOM.NOEUD', iret)
        if (iret .gt. 0) call jedetr('&&MSTGET.NOM.NOEUD')
        call jeexin('&&MSTGET.POSITION.NOEUD', iret)
        if (iret .gt. 0) call jedetr('&&MSTGET.POSITION.NOEUD')
        call jeexin('&&MSTGET.GROUP_NO', iret)
        if (iret .gt. 0) call jedetr('&&MSTGET.GROUP_NO')
        call jeexin('&&MSTGET.NOM.CMP', iret)
        if (iret .gt. 0) call jedetr('&&MSTGET.NOM.CMP')
!
 10     continue
    end do
    call jedetr('&&MSTGET.LISTE.LAGRAN')
    call jedetr('&&MSTGET.LISTE.BLOQUE')
    call jedetr('&&MSTGET.LISTE.ACTIF')
    call jedetr('&&MSTGET.LISTE.ACTBLO')
!
!
    call jedema()
end subroutine
