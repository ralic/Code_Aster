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
#include "asterf_types.h"
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
#include "asterfort/jexatr.h"
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
    character(len=8) :: ma, typem0, typemr
    aster_logical :: troisd
!
!
    integer :: nvoima, nscoma
    parameter(nvoima=200,nscoma=4)
    integer :: touvoi(1:nvoima, 1:nscoma+2)
    integer :: iv, nbma, dim, dimma, iatyma, m0, is, adcom0, nbsom0
    integer :: nbmr, admar, ir, numar, nvtot, iad, dimvlo, jnvge
    integer :: jconnex0, jconnexc, jconinv0, jconinvc
!
!
!
! --------- CONSTRUCTION DE LA CONNECTIVITE INVERSE --------------------
!
    call jemarq()
    call dismoi('NOM_MAILLA', ligrel, 'LIGREL', repk=ma)
    call dismoi('NB_MA_MAILLA', ma, 'MAILLAGE', repi=nbma)
!
    typmai=ma//'.TYPMAIL'
    connex=ma//'.CONNEX'
    call jeveuo(connex, 'L', jconnex0)
    call jeveuo(jexatr(connex, 'LONCUM'), 'L', jconnexc)
!
! --------- RECHERCHE DES EVENTUELLES MAILLES 3D DANS LE MODELE --------
!     SI ON EN TROUVE DIM=3 SINON DIM=2 (CE QUI EXCLUE DIM=1 !!!)
!
    troisd=.false.
    call jeveuo(typmai, 'L', iatyma)
    do m0 = 1, nbma
        iad=iatyma-1+m0
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(iad)), typem0)
        call dimmai(typem0, dimma)
        if (dimma .eq. 3) then
            troisd=.true.
            goto 20
!
        endif
    end do
 20 continue
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
    call cncinv(ma, [0], 0, 'G', coninv)
    call jeveuo(coninv, 'L', jconinv0)
    call jeveuo(jexatr(coninv, 'LONCUM'), 'L', jconinvc)
!
    typmai=ma//'.TYPMAIL'
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
!
!     DIMENSIONNEMENT DE L OBJET CONTENANT LES VOISINS : ELVOIS
!
!
! --------- BOUCLE SUR LES MAILLES  ----------------
!
    do m0 = 1, nbma
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(iatyma-1+m0)), typem0)
        call dimmai(typem0, dimma)
!
!  ON NE TRAITE QUE LES MAILLES DONT LA DIM EST CELLE DE L ESPACE
!
        if (dimma .eq. dim) then
! --------- REMISE ZERO TOUVOI
            nvtot=0
            do iv = 1, nvoima
                do is = 1, nscoma+2
                    touvoi(iv,is)=0
                end do
            end do
            adcom0=jconnex0-1+zi(jconnexc-1+m0)
            call nbsomm(typem0, nbsom0)
!
!      REMPLISSAGE DU TABLEAU DE POINTEURS
!
! --------- BOUCLE SUR LES SOMMETS  ----------------
!
            do is = 1, nbsom0
!               -- NBMR NOMBRE DE MAILLES RELIEES AU SOMMET
                admar=jconinv0-1+zi(jconinvc-1+zi(adcom0-1+is))
                nbmr=zi(jconinvc-1+zi(adcom0-1+is)+1) - zi(jconinvc-1+zi(adcom0-1+is))
                if (nbmr .gt. 1) then
                    do ir = 1, nbmr
!                       -- NUMAR NUMERO DE LA MAILLE RELIEE AU SOMMET
                        numar=zi(admar-1+ir)
                        call jenuno(jexnum( '&CATA.TM.NOMTM', zi(iatyma- 1+numar)), typemr)
                        call dimmai(typemr, dimma)
!
!                       ON NE STOQUE QUE LES VOISINS DE MEME DIMENTION
!
!
                        if (numar .ne. m0 .and. (dimma.eq.dim)) then
!                           -- AJOUT DE NUMAR A TOUVOI POUR M0
                            call adlivo(numar, is, nvtot, nvoima, nscoma,&
                                        touvoi)
                        endif
                    end do
                endif
            end do
            call dimvoi(nvtot, nvoima, nscoma, touvoi, dimvlo)
        else
!
!      POUR LES ELEMENTS NON TARITES ON ECRIT QUAND MEME
!      QUE LE NOMBRE DE VOISINS EST NUL
!
            dimvlo=1
        endif
        zi(iaddvo+m0)=zi(iaddvo+m0-1)+dimvlo
    end do
!
!  ON PEUT MAINTENANT ALLOUER ET REMPLIR CET OBJET
!
    call wkvect(elvois, bas1//' V I', zi(iaddvo+nbma), iadvoi)
! --------- BOUCLE SUR LES MAILLES  ----------------
!
    do m0 = 1, nbma
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(iatyma-1+m0)), typem0)
        call dimmai(typem0, dimma)
!
!  ON NE TRAITE QUE LES MAILLES DONT LA DIM EST CELLE DE L ESPACE
!
        if (dimma .eq. dim) then
! --------- REMISE ZERO TOUVOI
            nvtot=0
            do iv = 1, nvoima
                do is = 1, nscoma+2
                    touvoi(iv,is)=0
                end do
            end do
            adcom0=jconnex0-1+zi(jconnexc-1+m0)
            call nbsomm(typem0, nbsom0)
!
! --------- BOUCLE SUR LES SOMMETS  ----------------
!
            do is = 1, nbsom0
!               -- NBMR NOMBRE DE MAILLES RELIEES AU SOMMET
                admar=jconinv0-1+zi(jconinvc-1+zi(adcom0-1+is))
                nbmr=zi(jconinvc-1+zi(adcom0-1+is)+1) - zi(jconinvc-1+zi(adcom0-1+is))
                if (nbmr .gt. 1) then
                    do ir = 1, nbmr
!                       NUMAR NUMERO DE LA MAILLE RELIEE AU SOMMET
                        numar=zi(admar-1+ir)
                        call jenuno(jexnum( '&CATA.TM.NOMTM', zi(iatyma- 1+numar)), typemr)
                        call dimmai(typemr, dimma)
                        if (numar .ne. m0 .and. (dimma.eq.dim)) then
!                           -- AJOUT DE NUMAR A TOUVOI POUR M0
                            call adlivo(numar, is, nvtot, nvoima, nscoma,&
                                        touvoi)
                        endif
                    end do
                endif
            end do
            iad=iadvoi+zi(iaddvo+m0-1)
            call crvloc(dim, adcom0, iatyma, jconnex0, jconnexc,&
                        zi(iad), nvtot, nvoima, nscoma, touvoi)
        else
!
!           POUR LES ELEMENTS NON TARITES ON ECRIT QUAND MEME
!           QUE LE NOMBRE DE VOISINS EST NUL
!
            zi(iadvoi+zi(iaddvo+m0-1))=0
        endif
    end do
!
!
!   --  CALL IMPVOI(' VGE EN FIN DE CREVGE ',NBMA,IADDVO,IADVOI)
    call jedetr(coninv)
    call jedema()
!
end subroutine
