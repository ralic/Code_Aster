subroutine speph1(intphy, intmod, nomu, cham, specmr,&
                  specmi, nnoe, nomcmp, nbmode, nbn,&
                  nbpf)
    implicit   none
#include "jeveux.h"
!
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    logical :: intphy, intmod
    integer :: nbmode, nbn, nbpf
    real(kind=8) :: cham(nbn, *), specmr(nbpf, *), specmi(nbpf, *)
    character(len=8) :: nomu, nnoe(*), nomcmp(*)
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!  RESTITUTION SUR BASE PHYSIQUE D'UNE TABL_INTSP DE REPONSE MODALE
!  LA BASE MODALE EST DEFINIE PAR UN CONCEPT MODE_MECA
!-----------------------------------------------------------------------
! IN  : INTPHY : BOOLEEN
!                CARACTERISE LE CONTENU DE LA TABLE D'INTERSPECTRES DE
!                REPONSE PHYSIQUE A CALCULER
!       INTPHY = .TRUE.  TOUS LES INTERSPECTRES SERONT CALCULES
!       INTPHY = .FALSE. SEULS LES AUTOSPECTRES SERONT CALCULES
! IN  : INTMOD : BOOLEEN
!                CARACTERISE LE CONTENU DE LA TABLE D'INTERSPECTRES DE
!                REPONSE MODALE (DONNEE DU CALCUL)
!       INTMOD = .TRUE.  TOUS LES INTERSPECTRES ONT ETE CALCULES
!       INTMOD = .FALSE. SEULS LES AUTOSPECTRES ONT ETE CALCULES
! IN  : NOMU   : NOM UTILISATEUR DU CONCEPT TABL_INTSP DE REPONSE
!                PHYSIQUE : A PRODUIRE
! IN  : CHAM   : CHAMP DE GRANDEURS MODALES AUX NOEUDS DE REPONSE
! IN  : SPECMR : VECTEUR DE TRAVAIL
! IN  : SPECMI : VECTEUR DE TRAVAIL
! IN  : NNOE   : LISTE DES NOEUDS OU LA REPONSE EST CALCULEE
! IN  : NBMODE : NBR. DE MODES PRIS EN COMPTE
! IN  : NBN    : NBR. DE NOEUDS DE REPONSE
! IN  : NBPF   : NBR. DE POINTS DE LA DISCRETISATION FREQUENTIELLE
!-----------------------------------------------------------------------
!
    integer :: nbpar, inj, idebn, ini, il, im2, idebm, im1, ism
    integer :: nbabs, ispec, mxval, lnoei, lnoej, lcmpi, lcmpj, ij
    parameter   ( nbpar = 5 )
    real(kind=8) :: specr, speci
    character(len=24) :: kval(nbpar)
    character(len=24) :: chnoei, chnoej, chcmpi, chcmpj, chvals
!
!-----------------------------------------------------------------------
    call jemarq()
!
!    --- CREATION ET REMPLISSAGE DES FONCTIONS - SPECTRES REPONSES
!
    chnoei = nomu//'.NOEI'
    chnoej = nomu//'.NOEJ'
    chcmpi = nomu//'.CMPI'
    chcmpj = nomu//'.CMPJ'
    chvals = nomu//'.VALE'
!
    if (intphy) then
        mxval = nbn*(nbn+1)/2
    else
        mxval = nbn
    endif
!
    call wkvect(chnoei, 'G V K8', mxval, lnoei)
    call wkvect(chnoej, 'G V K8', mxval, lnoej)
    call wkvect(chcmpi, 'G V K8', mxval, lcmpi)
    call wkvect(chcmpj, 'G V K8', mxval, lcmpj)
    call jecrec(chvals, 'G V R', 'NU', 'DISPERSE', 'VARIABLE',&
                mxval)
!
    ij = 0
    do 60 inj = 1, nbn
!
        kval(3) = nnoe(inj)
        kval(4) = nomcmp(inj)
!
        idebn = inj
        if (intphy) idebn = 1
!
        do 70 ini = idebn, inj
!
            ij = ij+1
            kval(1) = nnoe(ini)
            kval(2) = nomcmp(ini)
!
            zk8(lnoei-1+ij) = kval(1)(1:8)
            zk8(lnoej-1+ij) = kval(3)(1:8)
            zk8(lcmpi-1+ij) = kval(2)(1:8)
            zk8(lcmpj-1+ij) = kval(4)(1:8)
!
            if ((kval(1) .eq. kval(3)) .and. (kval(2) .eq. kval(4))) then
                nbabs = nbpf
            else
                nbabs = 2*nbpf
            endif
!
            call jecroc(jexnum(chvals, ij))
            call jeecra(jexnum(chvals, ij), 'LONMAX', nbabs, ' ')
            call jeecra(jexnum(chvals, ij), 'LONUTI', nbabs, ' ')
            call jeveuo(jexnum(chvals, ij), 'E', ispec)
!
            do 90 il = 1, nbpf
!
                specr = 0.d0
                speci = 0.d0
!
                do 100 im2 = 1, nbmode
!
                    idebm = im2
                    if (intmod) idebm = 1
!
                    do 110 im1 = idebm, im2
                        ism = (im2* (im2-1))/2 + im1
!
                        if (im1 .eq. im2) then
!                 --------------------
!
                            specr = specr + cham(ini,im1)*cham(inj, im2)* specmr(il,ism)
!
!
                        else
!                 ----
!
                            specr = specr + cham(ini,im1)*cham(inj, im2)* specmr(il,ism) + cham(i&
                                    &ni,im2)* cham(inj,im1)*specmr(il,ism)
                            speci = speci + cham(ini,im1)*cham(inj, im2)* specmi(il,ism) - cham(i&
                                    &ni,im2)* cham(inj,im1)*specmi(il,ism)
!
                        endif
!                 -----
!
110                  continue
100              continue
!
                if ((kval(1) .eq. kval(3)) .and. (kval(2) .eq. kval(4) )) then
                    zr(ispec-1+il) = specr
                else
                    zr(ispec+2*(il-1) ) = specr
                    zr(ispec+2*(il-1)+1) = speci
                endif
90          continue
!
70      continue
!
60  continue
!
    call jedema()
end subroutine
