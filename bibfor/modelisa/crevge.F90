subroutine crevge(ligrel, bas1)
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
    implicit none
!-----------------------------------------------------------------------
! BUT : CREE UNE SD_VOISINAGE DANS UNE SD_LIGREL
!
!  IN/JXVAR   LIGREL      : LIGREL
!
!  LA STRUCTURE VGE CREE A LES LIMITES SUIVANTES :
!   ON NE TRAITE QUE LES ELEMENTS QUI ONT LA DIMENSION MAXIMALE :
!         3 S'IL Y A DES ELEMENTS 3D
!         2 S'IL Y A DES ELEMENTS 2D ET PAS DE 3D
!   POUR CHAQUE ELEMENT ON NE STOQUE QUE SES VOISINS DE MEME DIMENSION.
!   EN D AUTRES TERMES
!      EN DIMENSION 3 ON TRAITE LES TYPES
!           F3 (3D PAR FACE)  A3 (3D PAR ARRETE)  ET S3 (3D PAR SOMMET)
!      EN DIMENSION 2 ON TRAITE LES TYPES
!           A2  (2D PAR ARRETE)  ET S2 (2D PAR SOMMET)
!-----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/adlivo.h"
#include "asterfort/cncinv.h"
#include "asterfort/crvloc.h"
#include "asterfort/dimmai.h"
#include "asterfort/dimvoi.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcncon.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbsomm.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=19) :: ligrel
    character(len=1) :: bas1
    character(len=12) :: vge
    integer :: iaddvo, iadvoi
!
    character(len=24) :: typmai, connex, coninv, ptvois, elvois
    character(len=8) :: ma, kbid, typem0, typemr
    character(len=32) :: no
    logical :: troisd
!
!
    integer :: nvoima, nscoma
    parameter(nvoima=100,nscoma=4)
    integer :: touvoi(1:nvoima, 1:nscoma+2)
    integer :: iv, ibid, nbma, ier, dim, dimma, iatyma, m0, is, adcom0, nbsom0
    integer :: nbmr, admar, ir, numar, nvtot, iad, dimvlo, jnvge
!
!
!
! --------- CONSTRUCTION DE LA CONNECTIVITE INVERSE --------------------
!
    call jemarq()
    call dismoi('F', 'NOM_MAILLA', ligrel, 'LIGREL', ibid,&
                ma, ier)
    call dismoi('F', 'NB_MA_MAILLA', ma, 'MAILLAGE', nbma,&
                kbid, ier)
!
    typmai=ma//'.TYPMAIL'
    connex=ma//'.CONNEX'
!
! --------- RECHERCHE DES EVENTUELLES MAILLES 3D DANS LE MODELE --------
!     SI ON EN TROUVE DIM=3 SINON DIM=2 (CE QUI EXCLUE DIM=1 !!!)
!
    troisd=.false.
    call jeveuo(typmai, 'L', iatyma)
    do 10 m0 = 1, nbma
        iad=iatyma-1+m0
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(iad)), typem0)
        call dimmai(typem0, dimma)
        if (dimma .eq. 3) then
            troisd=.true.
            goto 20
!
        endif
10  end do
20  continue
    if (troisd) then
        dim=3
    else
        dim=2
    endif
    if (nbma .lt. 1) then
        call utmess('F', 'VOLUFINI_5', si=nbma)
    endif
!
! --------- CREATION DU POINTEUR DE LONGUEUR DE CONINV ----------------
!
    coninv='&&CREVGE.CONINV'
    call cncinv(ma, ibid, 0, 'G', coninv)
!
    typmai=ma//'.TYPMAIL'
    connex=ma//'.CONNEX'
!
    call wkvect(ligrel//'.NVGE', bas1//' V K16', 1, jnvge)
    call gcncon('_', vge(1:8))
    vge(9:12)='.VGE'
    zk16(jnvge-1+1)=vge
!
    ptvois=vge//'.PTVOIS'
    elvois=vge//'.ELVOIS'
!
!
    call wkvect(ptvois, bas1//' V I', nbma+1, iaddvo)
    zi(iaddvo)=0
!
!
    call jeveuo(typmai, 'L', iatyma)
!
!     DIMENSIONNEMENT DE L OBJET CONTENANT LES VOISINS : ELVOIS
!
!
! --------- BOUCLE SUR LES MAILLES  ----------------
!
    do 70 m0 = 1, nbma
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(iatyma-1+m0)), typem0)
        call dimmai(typem0, dimma)
!
!  ON NE TRAITE QUE LES MAILLES DONT LA DIM EST CELLE DE L ESPACE
!
        if (dimma .eq. dim) then
! --------- REMISE ZERO TOUVOI
            nvtot=0
            do 40 iv = 1, nvoima
                do 30 is = 1, nscoma+2
                    touvoi(iv,is)=0
30              continue
40          continue
            call jeveuo(jexnum(connex, m0), 'L', adcom0)
            call nbsomm(typem0, nbsom0)
!
!      REMPLISSAGE DU TABLEAU DE POINTEURS
!
! --------- BOUCLE SUR LES SOMMETS  ----------------
!
            do 60 is = 1, nbsom0
                no=jexnum(coninv,zi(adcom0-1+is))
!  NBMR NOMBRE DE MAILLES RELIEES AU SOMMET
                call jelira(no, 'LONMAX', nbmr)
                call jeveuo(no, 'L', admar)
                if (nbmr .gt. 1) then
                    do 50 ir = 1, nbmr
!  NUMAR NUMERO DE LA MAILLE RELIEE AU SOMMET
                        numar=zi(admar-1+ir)
                        call jenuno(jexnum( '&CATA.TM.NOMTM', zi(iatyma- 1+numar)), typemr)
                        call dimmai(typemr, dimma)
!
!  ON NE STOQUE QUE LES VOISINS DE MEME DIMENTION
!
!
                        if (numar .ne. m0 .and. (dimma.eq.dim)) then
!  -- AJOUT DE NUMAR A TOUVOI POUR M0
                            call adlivo(numar, is, nvtot, nvoima, nscoma,&
                                        touvoi)
                        endif
50                  continue
                endif
60          continue
            call dimvoi(nvtot, nvoima, nscoma, touvoi, dimvlo)
        else
!
!      POUR LES ELEMENTS NON TARITES ON ECRIT QUAND MEME
!      QUE LE NOMBRE DE VOISINS EST NUL
!
            dimvlo=1
        endif
        zi(iaddvo+m0)=zi(iaddvo+m0-1)+dimvlo
70  end do
!
!  ON PEUT MAINTENANT ALLOUER ET REMPLIR CET OBJET
!
    call wkvect(elvois, bas1//' V I', zi(iaddvo+nbma), iadvoi)
! --------- BOUCLE SUR LES MAILLES  ----------------
!
    do 120 m0 = 1, nbma
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(iatyma-1+m0)), typem0)
        call dimmai(typem0, dimma)
!
!  ON NE TRAITE QUE LES MAILLES DONT LA DIM EST CELLE DE L ESPACE
!
        if (dimma .eq. dim) then
! --------- REMISE ZERO TOUVOI
            nvtot=0
            do 90 iv = 1, nvoima
                do 80 is = 1, nscoma+2
                    touvoi(iv,is)=0
80              continue
90          continue
            call jeveuo(jexnum(connex, m0), 'L', adcom0)
            call nbsomm(typem0, nbsom0)
!
! --------- BOUCLE SUR LES SOMMETS  ----------------
!
            do 110 is = 1, nbsom0
                no=jexnum(coninv,zi(adcom0-1+is))
!  NBMR NOMBRE DE MAILLES RELIEES AU SOMMET
                call jelira(no, 'LONMAX', nbmr)
                call jeveuo(no, 'L', admar)
                if (nbmr .gt. 1) then
                    do 100 ir = 1, nbmr
!  NUMAR NUMERO DE LA MAILLE RELIEE AU SOMMET
                        numar=zi(admar-1+ir)
                        call jenuno(jexnum( '&CATA.TM.NOMTM', zi(iatyma- 1+numar)), typemr)
                        call dimmai(typemr, dimma)
                        if (numar .ne. m0 .and. (dimma.eq.dim)) then
!  -- AJOUT DE NUMAR A TOUVOI POUR M0
                            call adlivo(numar, is, nvtot, nvoima, nscoma,&
                                        touvoi)
                        endif
100                  continue
                endif
110          continue
            iad=iadvoi+zi(iaddvo+m0-1)
            call crvloc(dim, adcom0, iatyma, connex, zi(iad),&
                        nvtot, nvoima, nscoma, touvoi)
        else
!
!      POUR LES ELEMENTS NON TARITES ON ECRIT QUAND MEME
!      QUE LE NOMBRE DE VOISINS EST NUL
!
            zi(iadvoi+zi(iaddvo+m0-1))=0
        endif
120  end do
!
!
!      CALL IMPVOI(' VGE EN FIN DE CREVGE ',NBMA,IADDVO,IADVOI)
    call jedetr(coninv)
    call jedema()
!
end subroutine
