subroutine caechp(char, ligrch, ligrmo, igrel, inema,&
                  noma, fonree, ndim)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/nocart.h"
#include "asterfort/paligi.h"
#include "asterfort/palima.h"
#include "asterfort/patrma.h"
#include "asterfort/u2mesk.h"
#include "asterfort/xtempc.h"
#include "asterfort/xtmafi.h"
#include "asterfort/xvelfm.h"
!
    integer :: igrel, inema, ndim
    character(len=4) :: fonree
    character(len=8) :: char, noma
    character(len=*) :: ligrch, ligrmo
!---------------------------------------------------------------------
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
!
!     BUT: REMPLIR LA CARTE .HECHP ET LE LIGREL DE CHARGE POUR LE MOT
!     CLE ECHANGE_PAROI
!
! ARGUMENTS D'ENTREE:
! IN   CHAR   K8  : NOM UTILISATEUR DU RESULTAT DE CHARGE
! IN   LIGRCH K19 : NOM DU LIGREL DE CHARGE
! IN   LIGRMO K19 : NOM DU LIGREL DU MODELE
! IN   IGREL  I   : NUMERO DU GREL DE CHARGE
! VAR  INEMA  I   : NUMERO  DE LA DERNIERE MAILLE TARDIVE DANS LIGRCH
! IN   NOMA   K8  : NOM DU MAILLAGE
! IN   FONREE K4  : 'FONC' OU 'REEL'
!
    integer :: nbtymx, nechp, ibid, ierd, jncmp, jvalv, iocc, nh, nt, i, j
    integer :: nbtyp, jlistt, nbm, nfiss, nfismx, jma, ntcon
    parameter    (nfismx=100)
    logical :: ltcon, lcoefh
!-----------------------------------------------------------------------
    integer :: jligr, ncmp
!-----------------------------------------------------------------------
    parameter    (nbtymx=7)
! --- NOMBRE MAX DE TYPE_MAIL DE COUPLAGE ENTRE 2 PAROIS
    real(kind=8) :: t(3), cechpr
    character(len=8) :: mo, k8b, cechpf, fiss(nfismx)
    character(len=16) :: motclf
    character(len=24) :: liel, modl, llist1, llist2, llistt
    character(len=19) :: carte
    character(len=24) :: mesmai, lismai
!     ------------------------------------------------------------------
    call jemarq()
!
    motclf = 'ECHANGE_PAROI'
    call getfac(motclf, nechp)
    if (nechp .eq. 0) goto 9999
!
    liel = ligrch
    liel(20:24) = '.LIEL'
    mo = ligrmo
    call dismoi('F', 'MODELISATION', mo, 'MODELE', ibid,&
                modl, ierd)
!
!     LE MOT-CLE COEF_H EST-IL PRESENT ?
    lcoefh=.false.
    do 100 iocc = 1, nechp
        if (fonree .eq. 'REEL') then
            call getvr8(motclf, 'COEF_H', iocc=iocc, scal=cechpr, nbret=nh)
        else if (fonree.eq.'FONC') then
            call getvid(motclf, 'COEF_H', iocc=iocc, scal=cechpf, nbret=nh)
        endif
        if (nh .ne. 0) then
            lcoefh=.true.
            goto 200
        endif
100  end do
200  continue
!
!     SI LE MOT-CLE COEF_H EST PRESENT, ON ALLOUE ET PREAPRE LA CARTE
    if (lcoefh) then
        carte = char//'.CHTH.HECHP'
        if (fonree .eq. 'REEL') then
            call alcart('G', carte, noma, 'COEH_R')
        else if (fonree.eq.'FONC') then
            call alcart('G', carte, noma, 'COEH_F')
        else
            call u2mesk('F', 'MODELISA2_37', 1, fonree)
        endif
!       NOM DE LA CMP DU COEFFICIENT D'ECHANGE DANS LA CARTE
        call jeveuo(carte//'.NCMP', 'E', jncmp)
        call jeveuo(carte//'.VALV', 'E', jvalv)
        ncmp = 1
        zk8(jncmp) = 'H'
    endif
!
! ----------------------------------------------------------------------
! --- BOUCLE SUR LES OCCURENCES DU MCF
! ----------------------------------------------------------------------
    do 300 iocc = 1, nechp
!
!       RECUPERATION DU COEFFICIENT D'ECHANGE
        if (fonree .eq. 'REEL') then
            call getvr8(motclf, 'COEF_H', iocc=iocc, scal=cechpr, nbret=nh)
        else if (fonree.eq.'FONC') then
            call getvid(motclf, 'COEF_H', iocc=iocc, scal=cechpf, nbret=nh)
        endif
!
!       RECUPERATION DU VECTEUR DE TRANSLATION POUR PATRMA
        do 301 i = 1, 3
            t(i) = 0.0d0
301      continue
        call getvr8(motclf, 'TRAN', iocc=iocc, nbval=3, vect=t,&
                    nbret=nt)
        call getvid(motclf, 'FISSURE', iocc=iocc, nbval=0, nbret=nfiss)
!
! ----------------------------------------------------------------------
! ----- CAS MOT-CLEF FISSURE (X-FEM)
! ----------------------------------------------------------------------
        if (nfiss .ne. 0) then
!
!         RECUPERATION DU NOM DES FISSURES
            nfiss = -nfiss
            call getvid(motclf, 'FISSURE', iocc=iocc, nbval=nfiss, vect=fiss,&
                        nbret=ibid)
!         VERIFICATION DE LA COHERENCE ENTRE LES FISSURES ET LE MODELE
            call xvelfm(nfiss, fiss, ligrmo(1:8))
!
!         ON SCRUTE LE MC TEMP_CONTINUE
            ltcon=.false.
            call getvtx(motclf, 'TEMP_CONTINUE', iocc=iocc, scal=k8b, nbret=ntcon)
!         VERIF DE COHERENCE AVEC LE MC COEF_H
            if (ntcon .eq. 1) then
                ASSERT(k8b(1:3).eq.'OUI'.and. nh.eq.0)
                ltcon=.true.
            else
                ASSERT(nh.eq.1 .and. ntcon.eq.0)
            endif
!
! ----------------------------------------------------------------------
! ------- CAS TEMP_CONTINUE (X-FEM / PAS D'ECHANGE)
! ----------------------------------------------------------------------
            if (ltcon) then
!
                call xtempc(nfiss, fiss, fonree, char)
!
! ----------------------------------------------------------------------
! ------- CAS COEF_H (X-FEM / ECHANGE)
! ----------------------------------------------------------------------
            else
!
!           ON NOTE 0. OU '&FOZERO' DANS LA CARTE POUR TOUT LE MAILLAGE
                if (fonree .eq. 'REEL') then
                    zr(jvalv) = 0.d0
                else if (fonree.eq.'FONC') then
                    zk8(jvalv) = '&FOZERO'
                endif
                call nocart(carte, 1, ' ', 'NOM', 0,&
                            ' ', 0, ' ', ncmp)
!
!           RECUPERATION DES MAILLES PRINCIPALES XFEM POUR FISS(1:NFISS)
                mesmai = '&&CAECHP.MES_MAILLES'
                lismai = '&&CAECHP.NUM_MAILLES'
                call xtmafi(noma, ndim, fiss, nfiss, lismai,&
                            mesmai, nbm)
                call jeveuo(mesmai, 'L', jma)
!
!           STOCKAGE DANS LA CARTE SUR CES MAILLES
                if (fonree .eq. 'REEL') then
                    zr(jvalv) = cechpr
                else if (fonree.eq.'FONC') then
                    zk8(jvalv) = cechpf
                endif
                call nocart(carte, 3, ' ', 'NOM', nbm,&
                            zk8(jma), ibid, ' ', ncmp)
!
!           MENAGE
                call jedetr(mesmai)
                call jedetr(lismai)
!
            endif
!
! ----------------------------------------------------------------------
! ----- CAS MOTS-CLEFS GROUP_MA_1... (PAROI MAILLEE)
! ----------------------------------------------------------------------
        else
!
            llist1 = '&&CAECHP.LLIST1'
            llist2 = '&&CAECHP.LLIST2'
            llistt = '&&CAECHP.LLIST.TRIE'
!
            call palima(noma, motclf, 'GROUP_MA_1', 'MAILLE_1', iocc,&
                        llist1)
            call palima(noma, motclf, 'GROUP_MA_2', 'MAILLE_2', iocc,&
                        llist2)
!
            call patrma(llist1, llist2, t, nbtymx, noma,&
                        llistt, nbtyp)
!
!         MISE A JOUR DE LIGRCH ET STOCKAGE DANS LA CARTE
            do 400 j = 1, nbtyp
                igrel = igrel+1
                call jeveuo(jexnum(llistt, j), 'L', jlistt)
                call paligi('THER', modl, ligrch, igrel, inema,&
                            zi(jlistt))
!           STOCKAGE DANS LA CARTE
                call jeveuo(jexnum(liel, igrel), 'E', jligr)
                call jelira(jexnum(liel, igrel), 'LONMAX', nbm)
                nbm = nbm - 1
                if (fonree .eq. 'REEL') then
                    zr(jvalv) = cechpr
                else if (fonree.eq.'FONC') then
                    zk8(jvalv) = cechpf
                endif
                call nocart(carte, -3, ' ', 'NUM', nbm,&
                            ' ', zi(jligr), ligrch, ncmp)
400          continue
!
!         MENAGE
            call jedetr(llist1)
            call jedetr(llist2)
            call jedetr(llistt)
! ------
        endif
!
300  end do
!
9999  continue
    call jedema()
end subroutine
