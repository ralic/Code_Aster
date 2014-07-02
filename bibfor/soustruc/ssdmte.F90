subroutine ssdmte(mag)
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
    implicit none
!     ARGUMENTS:
!     ----------
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    character(len=8) :: mag
! ----------------------------------------------------------------------
!     BUT:
!        - TERMINER LE TRAITEMENT
!          DES COMMANDES DEFI_MAILLAGE ET CONC_MAILLAGE.
!        - CREER LES OBJETS :
!            BASE GLOBALE : .COORDO , .NOMNOE
!        - MODIFIER LES OBJETS :
!            BASE GLOBALE : .SUPMAIL, .GROUPENO ET .CONNEX
!            POUR TENIR COMPTE DES NOEUDS CONFONDUS.
!
!     IN:
!        MAG : NOM DU MAILLAGE RESULTAT.
!
    character(len=8) :: nomacr, nomnoe
    aster_logical :: recom
    character(len=19) :: coordo
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
!     NBNOPH : NOMBRE DE NOEUDS PHYSIQUES DE MAG "AVANT"
!              (AVANT DE CONFONDRE CERTAINS NOEUDS)
!     NBNOP2 : NOMBRE DE NOEUDS PHYSIQUES DE MAG "APRES"
!              (APRES AVOIR CONFONDU CERTAINS NOEUDS)
!     NBNOCO : NOMBRE DE NOEUDS CONFONDUS.
!              (NBNOCO=NBNOP2-NBNOPH)
!     NBNOLA : NOMBRE DE NOEUDS "LAGRANGE" DE MAG.
!     NBNOT2 : NOMBRE DE NOEUDS TOTAL DE MAG "APRES"
!              (APRES AVOIR CONFONDU CERTAINS NOEUDS)
!              (NBNOT2= NBNOLA+NBNOP2)
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, i2coex, iacoex, iadesc
    integer :: iagno, ianeno, iarefe
    integer :: iasupm, iatypl, iavale, ibid, ico, igeomr, igno
    integer :: ilcoex, ima, ino, iret, isma, jno, k
    integer :: kno, nbgno, nbma, nbno, nbnoco, nbnoe, nbnoet
    integer :: nbnogn, nbnol, nbnola, nbnop2, nbnoph, nbnot2, nbsma
    character(len=8), pointer :: vnomacr(:) => null()
    integer, pointer :: noeud_conf(:) => null()
    integer, pointer :: conx(:) => null()
    character(len=8), pointer :: nomnoe_2(:) => null()
    real(kind=8), pointer :: coordo_2(:) => null()
    integer, pointer :: dime(:) => null()
    integer, pointer :: dime_2(:) => null()
!
!-----------------------------------------------------------------------
    call jemarq()
    call jeveuo(mag//'.DIME', 'E', vi=dime)
    nbnoph= dime(1)
    nbnola= dime(2)
    nbma = dime(3)
    nbsma= dime(4)
    call jeveuo(mag//'.COORDO_2', 'L', vr=coordo_2)
    call jeveuo(mag//'.NOEUD_CONF', 'E', vi=noeud_conf)
    call jeveuo(mag//'.NOMNOE_2', 'L', vk8=nomnoe_2)
!
    if (nbsma .gt. 0) call jeveuo(mag//'.DIME_2', 'L', vi=dime_2)
    if (nbsma .gt. 0) call jeveuo(mag//'.NOMACR', 'L', vk8=vnomacr)
!
!
!     -- ON COMPTE LES NOEUDS PHYSIQUES REELLEMENT CONSERVES :
!     -------------------------------------------------------
    ico=0
    do 1 ino = 1, nbnoph
        if (noeud_conf(ino) .eq. ino) ico=ico+1
  1 end do
    nbnop2=ico
    nbnot2= nbnop2+nbnola
    nbnoco= nbnoph-nbnop2
!
    call jecreo(mag//'.NOMNOE', 'G N K8')
    call jeecra(mag//'.NOMNOE', 'NOMMAX', nbnot2)
!
!
!     -- CREATION DE .TYPL :
!     ----------------------
    if (nbnola .gt. 0) then
        call wkvect(mag//'.TYPL', 'G V I', nbnola, iatypl)
        do 2 ,isma=1,nbsma
        nomacr= vnomacr(isma)
        call jeveuo(nomacr//'.CONX', 'L', vi=conx)
        call jeveuo(jexnum(mag//'.SUPMAIL', isma), 'L', iasupm)
        nbnoe=dime_2(4*(isma-1)+1)
        nbnol=dime_2(4*(isma-1)+2)
        nbnoet= nbnoe+nbnol
        do 21 i = 1, nbnoet
            ino=zi(iasupm-1+i)
            if (ino .gt. nbnoph) then
                zi(iatypl-1+ino-nbnoph)= conx(3*(i-1)+3)
            endif
 21     continue
  2     continue
    endif
!
!
!     -- CREATION DU CHAMP .COORDO :
!     ------------------------------
    coordo= mag//'.COORDO'
!
    call jenonu(jexnom('&CATA.GD.NOMGD', 'GEOM_R'), igeomr)
    call wkvect(coordo//'.DESC', 'G V I', 3, iadesc)
    call jeecra(coordo//'.DESC', 'DOCU', ibid, 'CHNO')
    zi (iadesc-1+1)= igeomr
!     -- TOUJOURS 3 COMPOSANTES X, Y ET Z
    zi (iadesc-1+2)= -3
!     -- 14 = 2**1 + 2**2 + 2**3
    zi (iadesc-1+3)= 14
!
    call wkvect(coordo//'.REFE', 'G V K24', 4, iarefe)
    zk24(iarefe-1+1)= mag
    call wkvect(coordo//'.VALE', 'G V R', 3*nbnop2, iavale)
!     -- NOM DES NOEUDS PHYSIQUES (ET LEUR COORDONNEES) :
    ico=0
    do 3 , ino=1, nbnoph
    jno=noeud_conf(ino)
    if (ino .ne. jno) goto 3
    ico= ico+1
    if (nomnoe_2(ino) .ne. ' ') then
        nomnoe=nomnoe_2(ino)
    else
        nomnoe='N?'
        call codent(ico, 'G', nomnoe(2:8))
    endif
    call jecroc(jexnom(mag//'.NOMNOE', nomnoe))
    do 31 k = 1, 3
        zr(iavale-1+3*(ico-1)+k)=coordo_2(3*(ino-1)+k)
 31 continue
    3 end do
!     -- NOM DES NOEUDS DE LAGRANGE :
    nomnoe='&?'
    do 4 , ino=1, nbnola
    call codent(ino, 'G', nomnoe(2:8))
    call jecroc(jexnom(mag//'.NOMNOE', nomnoe))
    4 end do
!
!
!     -- ON OTE LA "RECURSIVITE" DE .NOEUD_CONF:
!     ------------------------------------------
  5 continue
    recom=.false.
    do 6 ,ino=1,nbnoph
    jno=noeud_conf(ino)
    if (jno .ne. ino) then
        ASSERT(jno.le.ino)
        kno=noeud_conf(jno)
        if (kno .ne. jno) then
            noeud_conf(ino)= kno
            recom= .true.
        endif
    endif
    6 end do
    if (recom) goto 5
!
!
!     -- ON COMPACTE LES NUMEROS DES NOEUDS CONSERVES:
!     ------------------------------------------------
    call wkvect(mag//'.NENO', 'V V I', nbnoph, ianeno)
    ico = 0
    do 7 ,ino=1,nbnoph
    jno=noeud_conf(ino)
    if (jno .eq. ino) then
        ico= ico+1
        zi(ianeno-1+ino)=ico
    endif
    7 end do
!
!
!     -- MODIFICATION DES OBJETS POUR TENIR COMPTE DE .NOEUD_CONF:
!     -------------------------------------------------------------
!
!     -- MODIFICATION DE .CONNEX:
!     ---------------------------
    if (nbma .gt. 0) then
        call jeveuo(mag//'.CONNEX', 'E', iacoex)
        call jeveuo(jexatr(mag//'.CONNEX', 'LONCUM'), 'L', ilcoex)
    endif
    do 81 ima = 1, nbma
        nbno= zi(ilcoex-1+ima+1)-zi(ilcoex-1+ima)
        i2coex=iacoex-1+zi(ilcoex-1+ima)
        do 811 i = 1, nbno
            ino=zi(i2coex-1+i)
            if (ino .le. nbnoph) then
                jno=zi(ianeno-1+ noeud_conf(ino))
                zi(i2coex-1+i)=jno
            else
                zi(i2coex-1+i)=ino-nbnoco
            endif
811     continue
 81 end do
!
!     -- MODIFICATION DE .SUPMAIL:
!     ----------------------------
    do 82 isma = 1, nbsma
        call jeveuo(jexnum(mag//'.SUPMAIL', isma), 'E', iasupm)
        nbnoe=dime_2(4*(isma-1)+1)
        nbnol=dime_2(4*(isma-1)+2)
        nbnoet= nbnoe+nbnol
        do 821 i = 1, nbnoet
            ino=zi(iasupm-1+i)
            if (ino .le. nbnoph) then
                jno=zi(ianeno-1+ noeud_conf(ino))
                zi(iasupm-1+i)=jno
            else
                zi(iasupm-1+i)=ino-nbnoco
            endif
821     continue
 82 end do
!
!     -- MODIFICATION DE .GROUPENO:
!     ----------------------------
    call jeexin(mag//'.GROUPENO', iret)
    if (iret .gt. 0) then
        call jelira(mag//'.GROUPENO', 'NUTIOC', nbgno)
        do 9 ,igno=1,nbgno
        call jeveuo(jexnum(mag//'.GROUPENO', igno), 'E', iagno)
        call jelira(jexnum(mag//'.GROUPENO', igno), 'LONUTI', nbnogn)
        do 91 i = 1, nbnogn
            ino=zi(iagno-1+i)
            jno=zi(ianeno-1+ noeud_conf(ino))
            zi(iagno-1+i)=jno
 91     continue
  9     continue
    endif
!
!
!     -- REMISE A JOUR DEFINITIVE DU NOMBRE DE NOEUDS PHYSIQUES:
!     ----------------------------------------------------------
    dime(1)=nbnop2
!
!
    call jedema()
end subroutine
