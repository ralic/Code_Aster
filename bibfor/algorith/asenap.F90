subroutine asenap(masse)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/r8vide.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/reliem.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: masse
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     COMMANDE : COMB_SISM_MODAL POUR MULTI-SUPPORT UNIQUEMENT
!        VERIFIE QUE LES MODES STATIQUES SONT DEFINIS AUX SUPPORTS,
!                    OPTION REAC_NODA CALCULEE DANS LES MODES MECANIQUES
!        RECUPERATION DES TYPES DE COMBINAISON DES SUPPORTS,
!                     DES DEPLACEMENTS DES SUPPORTS
!   DANS CETTE ROUTINE ON CREE UN SERIE DE COLLECTIONS
!   - LISTE_CAS :
!
!     ------------------------------------------------------------------
! IN  : MASSE  : MATRICE DE MASSE DE LA STRUCTURE
!     ------------------------------------------------------------------
    integer :: ibid, icas, ier, ino, iocc, ire1, iref, iret, jcas, jdir
    integer :: jdref, jno, jnoeu, jnref, jref, jsta, jtyp, nbmc, nbno, nbocc, nc
    integer :: ncas, nocas, ns, nt, nucas, nx, ny, nz
    real(kind=8) :: dx, dy, dz, epsima
    character(len=4) :: ctyp
    character(len=8) :: k8b, resu, noma
    character(len=8) :: knum, kdir, stat, motcle(2), tymocl(2)
    character(len=15) :: motfac
    character(len=16) :: concep, nomcmd, mesnoe
    character(len=24) :: obj2, valk(2), noref
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
!
    epsima =r8vide()
    noref=' '
!
    call getres(resu, concep, nomcmd)
!
    call dismoi('F', 'NOM_MAILLA', masse, 'MATR_ASSE', ibid,&
                noma, iret)
    obj2 = noma//'.NOMNOE'
    ier = 0
!
!     --- RECUPERATION DES DEPLACEMENTS DES SUPPORTS ---
!
    motfac = 'COMB_DEPL_APPUI'
    call getfac(motfac, nbocc)
!
! -- CREATION DE LA COLLECTION LIST_CAS DE TOUTES LES OCCURRENCES
! -- DE COMB_DEPL_APPUI
!
    ncas = 0
    call jecrec('&&ASENAP.LISTCAS', 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                nbocc)
!
    do 10 iocc = 1, nbocc
        call getvtx(motfac, 'TOUT', iocc=iocc, nbval=0, nbret=nt)
        if (nt .ne. 0) then
            call getfac('DEPL_MULT_APPUI', ncas)
            if (ncas .lt. 2) then
                call u2mess('F', 'SEISME_21')
            endif
            call jecroc(jexnum('&&ASENAP.LISTCAS', iocc))
            call jeecra(jexnum('&&ASENAP.LISTCAS', iocc), 'LONMAX', ncas)
            call jeveuo(jexnum('&&ASENAP.LISTCAS', iocc), 'E', jcas)
            do 12 icas = 1, ncas
                call getvis('DEPL_MULT_APPUI', 'NUME_CAS', iocc=icas, scal=nucas, nbret=ibid)
                zi(jcas+icas-1) = nucas
12          continue
        else
            call getvis(motfac, 'LIST_CAS', iocc=iocc, nbval=0, nbret=nc)
            nc=-nc
            if (nc .lt. 2) then
                call u2mess('F', 'SEISME_22')
            endif
            call jecroc(jexnum('&&ASENAP.LISTCAS', iocc))
            call jeecra(jexnum('&&ASENAP.LISTCAS', iocc), 'LONMAX', nc)
            call jeveuo(jexnum('&&ASENAP.LISTCAS', iocc), 'E', jcas)
            call getvis(motfac, 'LIST_CAS', iocc=iocc, nbval=nc, vect=zi(jcas),&
                        nbret=nc)
            ncas =ncas+nc
        endif
10  end do
!
!
! -- CREATION DU VECTEUR TYPE_COMBI DE TOUTES LES OCCURRENCES
! -- DE COMB_DEPL_APPUI
!
    call wkvect('&&ASENAP.TYPE', 'V V I', nbocc, jtyp)
!
    do 20 iocc = 1, nbocc
        call getvtx(motfac, 'TYPE_COMBI', iocc=iocc, scal=ctyp, nbret=nc)
        if (ctyp .eq. 'QUAD') zi(jtyp+iocc-1) = 1
        if (ctyp .eq. 'LINE') zi(jtyp+iocc-1) = 2
        if (ctyp .eq. 'ABS') zi(jtyp+iocc-1) = 3
!
20  end do
!
!
! -- CREATION DE LA COLLECTION QUI CONTIENT LES NOEUDS
! -- DES DIFFERENTES OCCURRENCES DE DEPL_MULT_APPUI TRAITEES
!
    motfac = 'DEPL_MULT_APPUI'
    call getfac(motfac, nocas)
    call jecrec('&&ASENAP.LINOEU ', 'V V K8', 'NO', 'DISPERSE', 'VARIABLE',&
                nocas)
!
    call jecrec('&&ASENAP.LIDIR ', 'V V R', 'NO', 'DISPERSE', 'VARIABLE',&
                nocas)
! VECTEUR MODE_STATIQUE
    call wkvect('&&ASENAP.STAT', 'V V K8', nocas, jsta)
!
! VECTERUS RELATIFS AU NOEUD_REFE
!
    call wkvect('&&ASENAP.NOREF', 'V V K24', nocas, jnref)
    call wkvect('&&ASENAP.NREF', 'V V I', nocas, jref)
    call wkvect('&&ASENAP.DREF', 'V V R', 3*nocas, jdref)
!
    mesnoe = '&&ASENAP.NOEUDS'
    do 30 icas = 1, nocas
        call getvis(motfac, 'NUME_CAS', iocc=icas, scal=nucas, nbret=nc)
!
! INITIALISATION DU DEPLACEMENT DE  NOEUD_REFE
!
!
! -- STOCKAGE MODE STATIQUE DU NUME_CAS TRAITE
        call getvid(motfac, 'MODE_STAT', iocc=icas, scal=stat, nbret=ns)
        zk8(jsta+icas-1)=stat
! -- STOCKAGE DES NOEUD
        knum = 'N       '
        call codent(nucas, 'D0', knum(2:8))
        nbmc=2
        motcle(1) = 'NOEUD'
        tymocl(1) = 'NOEUD'
        motcle(2) = 'GROUP_NO'
        tymocl(2) = 'GROUP_NO'
        call reliem(' ', noma, 'NO_NOEUD', motfac, icas,&
                    nbmc, motcle, tymocl, mesnoe, nbno)
        call jeveuo(mesnoe, 'L', jnoeu)
!
        call jecroc(jexnom('&&ASENAP.LINOEU', knum))
        call jeecra(jexnom('&&ASENAP.LINOEU', knum), 'LONMAX', nbno)
        call jeecra(jexnom('&&ASENAP.LINOEU', knum), 'LONUTI', nbno)
        call jeveuo(jexnom('&&ASENAP.LINOEU', knum), 'E', jno)
        do 34 ino = 1, nbno
            zk8(jno+ino-1) = zk8(jnoeu+ino-1)
34      continue
! -- STOCKAGE DES NOEUD REFE
        zi(jref+icas-1)= 0
!
        zr(jdref+icas-1) = 0.0d0
        zr(jdref+icas+1-1) = 0.0d0
        zr(jdref+icas+2-1) = 0.0d0
        call getvtx(motfac, 'NOEUD_REFE', iocc=icas, scal=noref, nbret=iref)
        if (iref .ne. 0) then
            call jenonu(jexnom(obj2, noref), ire1)
            if (ire1 .eq. 0) then
                ier = ier + 1
                valk(1) = noref
                valk(2) = noma
                call u2mesk('E', 'SEISME_1', 2, valk)
                goto 9999
            endif
!
            zk24(jnref+icas-1) = noref
            zi(jref+icas-1)= 1
        endif
! -- STOCKAGE DES DIRECTIONS D''ANCRAGE
!
        kdir = 'D       '
        call codent(nucas, 'D0', kdir(2:8))
        call jecroc(jexnom('&&ASENAP.LIDIR', kdir))
        call jeecra(jexnom('&&ASENAP.LIDIR', kdir), 'LONMAX', 3*nbno)
        call jeecra(jexnom('&&ASENAP.LIDIR', kdir), 'LONUTI', 3*nbno)
        call jeveuo(jexnom('&&ASENAP.LIDIR', kdir), 'E', jdir)
        do 36 ino = 1, 3*nbno
            zr(jdir+ino-1)= epsima
36      continue
        call getvr8(motfac, 'DX', iocc=icas, scal=dx, nbret=nx)
        call getvr8(motfac, 'DY', iocc=icas, scal=dy, nbret=ny)
        call getvr8(motfac, 'DZ', iocc=icas, scal=dz, nbret=nz)
!
        do 38 ino = 1, nbno
            if (nx .ne. 0) zr(jdir+3*(ino-1))= dx
            if (ny .ne. 0) zr(jdir+3*(ino-1)+1) = dy
            if (nz .ne. 0) zr(jdir+3*(ino-1)+2)= dz
!
            if (zk8(jno+ino-1) .eq. noref) then
                zr(jdref+icas-1) = dx
                zr(jdref+icas+1-1) = dy
                zr(jdref+icas+2-1) = dz
            endif
            if (zi(jref+icas-1) .eq. 1) then
                zr(jdir+3*(ino-1))=zr(jdir+3*(ino-1))-zr(jdref+icas-1)
                zr(jdir+3*(ino-1)+1)=zr(jdir+3*(ino-1)+1)-zr(jdref+&
                icas+1-1)
                zr(jdir+3*(ino-1)+2)=zr(jdir+3*(ino-1)+2)-zr(jdref+&
                icas+2-1)
            endif
!
38      continue
30  end do
!
!
    call jedetr(mesnoe)
!
9999  continue
    if (ier .ne. 0) call u2mess('F', 'SEISME_6')
!
    call jedema()
end subroutine
