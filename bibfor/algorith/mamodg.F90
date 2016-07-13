subroutine mamodg(model, stolci, nomres, itxsto, itysto,&
                  itzsto, iprsto, iadirg, nbmo, max,&
                  may, maz, nbloc)
    implicit none
!---------------------------------------------------------------------
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
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "blas/ddot.h"
!
    integer :: nbpres, imatx, imaty, itxsto, itysto, itzsto
    integer ::  iprsto,   imatz
    integer ::  irang, jrang, i, j, iblo, ldblo,   iadirg
    integer :: iblodi,  nbloc, n1bloc, n2bloc, nbmo, nn
    integer :: ifm, niv, iret1, hc
    real(kind=8) :: mij, rx, ry, rz
    character(len=2) :: model
    character(len=8) :: repon
    character(len=8) :: nomres
    character(len=19) :: max, may, maz, stolci
    real(kind=8), pointer :: vectx(:) => null()
    real(kind=8), pointer :: vecty(:) => null()
    real(kind=8), pointer :: vectz(:) => null()
    integer, pointer :: smdi(:) => null()
    integer, pointer :: smde(:) => null()
    integer, pointer :: indic(:) => null()
    integer(kind=4), pointer :: smhc(:) => null()
    real(kind=8), pointer :: pres(:) => null()
    real(kind=8), pointer :: tpx(:) => null()
    real(kind=8), pointer :: tpy(:) => null()
    real(kind=8), pointer :: tpz(:) => null()
! ------------------------------------------------------------------
!----- ICI ON CALCULE LA MASSE AJOUTEE SUR UN MODELE GENERALISE ---
!
    call jemarq()
!
    call infniv(ifm, niv)
    call getvtx(' ', 'AVEC_MODE_STAT', scal=repon, nbret=nn)
    if (repon(1:3) .eq. 'NON') call jeveuo('&&DELAT.INDIC', 'L', vi=indic)
!
    call jeexin(stolci//'.SMHC', iret1)
    ASSERT(iret1.gt.0)
    call jeveuo(stolci//'.SMHC', 'L', vi4=smhc)
    call jeveuo(stolci//'.SMDI', 'L', vi=smdi)
    call jeveuo(stolci//'.SMDE', 'L', vi=smde)
!
    call jelira(zk24(iprsto)(1:19)//'.VALE', 'LONMAX', nbpres)
    AS_ALLOCATE(vr=vectx, size=nbpres)
    AS_ALLOCATE(vr=vecty, size=nbpres)
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
        AS_ALLOCATE(vr=vectz, size=nbpres)
    endif
!
!     BOUCLE SUR LES BLOCS DE LA MATRICE ASSEMBLEE GENE
!
    do iblo = 1, nbloc
!
        call jecroc(jexnum(nomres//'           .UALF', iblo))
        call jeveuo(jexnum(nomres//'           .UALF', iblo), 'E', ldblo)
!-------------------------------------------------------------
!
!         BOUCLE SUR LES COLONNES DE LA MATRICE ASSEMBLEE
!
        n1bloc=1
        n2bloc=smde(1)
!
!
        do 10 i = n1bloc, n2bloc
            if (i .gt. nbmo) goto 10
            if (repon(1:3) .eq. 'NON') then
                if (indic(i) .ne. 1) goto 10
            endif
            call jeveuo(zk24(itxsto+i-1)(1:19)//'.VALE', 'L', vr=tpx)
            call jeveuo(zk24(itysto+i-1)(1:19)//'.VALE', 'L', vr=tpy)
            if (model .eq. '3D') then
                call jeveuo(zk24(itzsto+i-1)(1:19)//'.VALE', 'L', vr=tpz)
                call mrmult('ZERO', imatz, tpz, vectz, 1,&
                            .true._1)
            endif
!
!------MULTIPLICATIONS MATRICE MAX * CHAMNO MODX---------------------
!----------ET MATRICE MAY * CHAMNO MODY------------------------------
!
            call mrmult('ZERO', imatx, tpx, vectx, 1,&
                        .true._1)
            call mrmult('ZERO', imaty, tpy, vecty, 1,&
                        .true._1)
!
! RANG GENERALISE DU TERME DE MASSE CALCULEE : LIGNE
!
            irang=zi(iadirg+i-1)
            hc = smdi(i)
            if (i .gt. 1) hc = hc - smdi(i-1)
!
            do 30 j = (i-hc+1), i
!
!----------------------------------------------------------------
! ICI ON CALCULE LA MASSE AJOUTEE SUR UN MODELE GENERALISE
!--------------------------------------------------------------
!-----------STOCKAGE DANS LA MATR_ASSE_GENE  ------
!
                if (repon(1:3) .eq. 'NON') then
                    if (indic(j) .ne. 1) goto 50
                endif
!
                call jeveuo(zk24(iprsto+j-1)(1:19)//'.VALE', 'L', vr=pres)
!
                rx= ddot(nbpres,pres, 1,vectx,1)
                ry= ddot(nbpres,pres, 1,vecty,1)
!
                if (model .eq. '3D') then
                    rz= ddot(nbpres,pres, 1,vectz,1)
                    mij = rx+ry+rz
                else
                    mij = rx+ry
                endif
50              continue
                if (repon(1:3) .eq. 'NON') then
                    if (indic(j) .ne. 1) mij=0.d0
                endif
!
! RANG GENERALISE DU TERME DE MASSE: COLONNE
!
                jrang=zi(iadirg+j-1)
                iblodi = 1
!
                if (iblodi .ne. iblo) then
!
!                 CAS OU LE BLOC COURANT N EST PAS LE BON
!
                    call jelibe(jexnum(nomres//'           .UALF', iblo))
                    call jeveuo(jexnum(nomres//'           .UALF', iblodi), 'E', ldblo)
                    zr(ldblo+smdi(irang)+jrang-irang-1) = mij
                    if (niv .eq. 2) then
                        write(ifm,350) irang,jrang,mij
                    endif
                    call jelibe(jexnum(nomres//'           .UALF', iblodi))
                    call jeveuo(jexnum(nomres//'           .UALF', iblo), 'E', ldblo)
!
                else
                    zr(ldblo+smdi(irang)+jrang-irang-1) = mij
                    if (niv .eq. 2) then
                        write(ifm,350) irang,jrang,mij
                    endif
                endif
30          continue
10      continue
    end do
!
    350 format(18x,'M',2 i 4,1x,'=',1x, d 12.5)
!
!
!--MENAGE FINAL DES OBJETS DE TRAVAIL
!
    AS_DEALLOCATE(vr=vectz)
    AS_DEALLOCATE(vr=vectx)
    AS_DEALLOCATE(vr=vecty)
!
    call jedema()
end subroutine
