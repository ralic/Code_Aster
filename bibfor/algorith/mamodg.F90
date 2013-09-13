subroutine mamodg(model, stolci, nomres, itxsto, itysto,&
                  itzsto, iprsto, iadirg, nbmo, max,&
                  may, maz, nbloc)
    implicit none
!---------------------------------------------------------------------
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
!---------------------------------------------------------------------
! ROUTINE NOUVEAU MODELE OPTIMISEE
! ROUTINE CALCULANT LA MASSE AJOUTEE SUR MODELE GENERALISE
! ARGUMENTS :
! IN : STOLCI : K19 : NOM CONCERNANT LES SD NUMEDDLGENE
! IN : NOMRES : K8 :NOM UTILISATEUR DU RESULTAT
! IN : MODEL : K2 : CHARACTER DISTINGUANT LE FLUIDE 2D ET 3D
! IN : MAX, MAY,MAZ : K19 : MATRICES AX, AY, AZ CALCULEES SUR
!                          L INTERFACE
! IN : ITXSTO,ITYSTO,ITZSTO,IPRSTO : ADR JEVEUX DES NOMS DES
!      CHAMPS DE DEPL_R STOCKEES PAR CMP ET DE LA PRESSION
!      CALCULEE SUR TOUS LES MODES
! IN : IADIRG : ADRESSE DU PREMIER ELEMENT D UN TABLEAU CONTENANT
!       LES RANGS GENERALISES DU COEFF DE MASSE AJOUTEE
! IN : NBMO : NOMBRE DE MODES TOTAL DANS LA BASE MODALE DES
!            SOUS-STRUCTURES - DEFORMEES STATIQUES + MODES
!             NORMAUX
!---------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtdscr.h"
#include "asterfort/wkvect.h"
#include "blas/ddot.h"
!
    integer :: nbpres, imatx, imaty, itxsto, itysto, itzsto, idelat
    integer :: ivx, ivy, itpx, itpy, ipres, iprsto, iadia, ihcol, imatz
    integer :: iablo, irang, jrang, i, j, iblo, ldblo, ivz, itpz, iadirg
    integer :: iblodi, ldiabl, nbloc, n1bloc, n2bloc, nbmo, nn
    integer :: ifm, niv, iret1
    real(kind=8) :: mij, rx, ry, rz
    character(len=2) :: model
    character(len=8) :: repon
    character(len=8) :: nomres
    character(len=19) :: max, may, maz, stolci
! ------------------------------------------------------------------
!----- ICI ON CALCULE LA MASSE AJOUTEE SUR UN MODELE GENERALISE ---
!
    call jemarq()
!
    call infniv(ifm, niv)
    call getvtx(' ', 'AVEC_MODE_STAT', scal=repon, nbret=nn)
    if (repon(1:3) .eq. 'NON') call jeveuo('&&DELAT.INDIC', 'L', idelat)
!
    call jeexin(stolci//'.SCHC', iret1)
    ASSERT(iret1.gt.0)
    call jeveuo(stolci//'.SCHC', 'L', ihcol)
    call jeveuo(stolci//'.SCDI', 'L', iadia)
    call jeveuo(stolci//'.SCBL', 'L', iablo)
    call jeveuo(stolci//'.SCIB', 'L', ldiabl)
!
    call jelira(zk24(iprsto)(1:19)//'.VALE', 'LONMAX', nbpres)
    call wkvect('&&MAMODG.VECTX', 'V V R', nbpres, ivx)
    call wkvect('&&MAMODG.VECTY', 'V V R', nbpres, ivy)
!
! --- RECUPERATION DES DESCRIPTEURS DE MATRICES ASSEMBLEES MAX ET MAY
! --- EVENTUELLEMENT MAZ
!
    call mtdscr(max)
    call jeveuo(max(1:19)//'.&INT', 'E', imatx)
    call mtdscr(may)
    call jeveuo(may(1:19)//'.&INT', 'E', imaty)
    if (model .eq. '3D') then
        call mtdscr(maz)
        call jeveuo(maz(1:19)//'.&INT', 'E', imatz)
        call wkvect('&&MAMODG.VECTZ', 'V V R', nbpres, ivz)
    endif
!
!     BOUCLE SUR LES BLOCS DE LA MATRICE ASSEMBLEE GENE
!
    do 40 iblo = 1, nbloc
!
        call jecroc(jexnum(nomres//'           .UALF', iblo))
        call jeveuo(jexnum(nomres//'           .UALF', iblo), 'E', ldblo)
!-------------------------------------------------------------
!
!         BOUCLE SUR LES COLONNES DE LA MATRICE ASSEMBLEE
!
        n1bloc=zi(iablo+iblo-1)+1
        n2bloc=zi(iablo+iblo)
!
!
        do 10 i = n1bloc, n2bloc
            if (i .gt. nbmo) goto 10
            if (repon(1:3) .eq. 'NON') then
                if (zi(idelat+i-1) .ne. 1) goto 10
            endif
            call jeveuo(zk24(itxsto+i-1)(1:19)//'.VALE', 'L', itpx)
            call jeveuo(zk24(itysto+i-1)(1:19)//'.VALE', 'L', itpy)
            if (model .eq. '3D') then
                call jeveuo(zk24(itzsto+i-1)(1:19)//'.VALE', 'L', itpz)
                call mrmult('ZERO', imatz, zr(itpz), zr(ivz), 1,&
                            .true.)
            endif
!
!------MULTIPLICATIONS MATRICE MAX * CHAMNO MODX---------------------
!----------ET MATRICE MAY * CHAMNO MODY------------------------------
!
            call mrmult('ZERO', imatx, zr(itpx), zr(ivx), 1,&
                        .true.)
            call mrmult('ZERO', imaty, zr(itpy), zr(ivy), 1,&
                        .true.)
!
! RANG GENERALISE DU TERME DE MASSE CALCULEE : LIGNE
!
            irang=zi(iadirg+i-1)
!
            do 30 j = (i-zi(ihcol+i-1)+1), i
!
!----------------------------------------------------------------
! ICI ON CALCULE LA MASSE AJOUTEE SUR UN MODELE GENERALISE
!--------------------------------------------------------------
!-----------STOCKAGE DANS LA MATR_ASSE_GENE  ------
!
                if (repon(1:3) .eq. 'NON') then
                    if (zi(idelat+j-1) .ne. 1) goto 50
                endif
!
                call jeveuo(zk24(iprsto+j-1)(1:19)//'.VALE', 'L', ipres)
!
                rx= ddot(nbpres,zr(ipres), 1,zr(ivx),1)
                ry= ddot(nbpres,zr(ipres), 1,zr(ivy),1)
!
                if (model .eq. '3D') then
                    rz= ddot(nbpres,zr(ipres), 1,zr(ivz),1)
                    mij = rx+ry+rz
                else
                    mij = rx+ry
                endif
50              continue
                if (repon(1:3) .eq. 'NON') then
                    if (zi(idelat+j-1) .ne. 1) mij=0.d0
                endif
!
! RANG GENERALISE DU TERME DE MASSE: COLONNE
!
                jrang=zi(iadirg+j-1)
                iblodi = zi(ldiabl+irang-1)
!
                if (iblodi .ne. iblo) then
!
!                 CAS OU LE BLOC COURANT N EST PAS LE BON
!
                    call jelibe(jexnum(nomres//'           .UALF', iblo))
                    call jeveuo(jexnum(nomres//'           .UALF', iblodi), 'E', ldblo)
                    zr(ldblo+zi(iadia+irang-1)+jrang-irang-1) = mij
                    if (niv .eq. 2) then
                        write(ifm,350) irang,jrang,mij
                    endif
                    call jelibe(jexnum(nomres//'           .UALF', iblodi))
                    call jeveuo(jexnum(nomres//'           .UALF', iblo), 'E', ldblo)
!
                else
                    zr(ldblo+zi(iadia+irang-1)+jrang-irang-1) = mij
                    if (niv .eq. 2) then
                        write(ifm,350) irang,jrang,mij
                    endif
                endif
30          continue
10      continue
40  end do
!
    350 format(18x,'M',2 i 4,1x,'=',1x, d 12.5)
!
!
!--MENAGE FINAL DES OBJETS DE TRAVAIL
!
    call jedetr('&&MAMODG.VECTZ')
    call jedetr('&&MAMODG.VECTX')
    call jedetr('&&MAMODG.VECTY')
!
    call jedema()
end subroutine
