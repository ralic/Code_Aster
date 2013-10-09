subroutine rotlis(nomres, fmli, icar, fplin, fplio,&
                  ii, sst, intf, fact)
    implicit none
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
!***********************************************************************
!  P. RICHARD     DATE 13/10/92
!-----------------------------------------------------------------------
!  BUT : < CALCUL DES LIAISONS >
!
!  CALCULER LES NOUVELLES MATRICES DE LIAISON EN TENANT COMPTE DE
!  L'ORIENTATION DES SOUS-STRUCTURES
!  ON DETERMINE LES MATRICES DE LIAISON, LES DIMENSIONS DE CES MATRICES
!  ET LE PRONO ASSOCIE
!  PRISE EN COMPTE D'UN FACTEUR MULTIPLICATIF POUR LA MATRICE ORIENTEE
!
!-----------------------------------------------------------------------
!
! NOMRES /I/ : NOM UTILISATEUR DU RESULTAT MODELE_GENE
! II     /I/ : NUMERO INTERFACE COURANTE
! FMLI   /I/ : FAMILLE DES MATRICES DE LIAISON
! ICAR   /I/ : CARACTERISTIQUE DE LA LIAISON
! FPLIN  /I/ : FAMILLE DES PROFNO DES MATRICES DE LIAISON NON ORIENTEES
! FPLIO  /I/ : FAMILLE DES PROFNO DES MATRICES DE LIAISON ORIENTEES
! SST    /I/ : NOM DE LA SOUS-STRUCTURE MISE EN JEU PAR LA LIAISON
! INTF   /I/ : NOM DE L'INTERFACE MISE EN JEU PAR LA LIAISON
! FACT   /I/ : FACTEUR REEL MULTIPLICATIF DE LA MATRICE DE LIAISON
!
!
!
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/exmali.h"
#include "asterfort/intet0.h"
#include "asterfort/isdeco.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mgutdm.h"
#include "asterfort/pmppr.h"
#include "asterfort/r8inir.h"
#include "asterfort/utmess.h"
!
!
!
!   PARAMETER REPRESENTANT LE NOMBRE MAX DE COMPOSANTES DE LA GRANDEUR
!   SOUS-JACENTE TRAITEE
!
    integer :: nbcmpm, ordo, nbec, ibid, llrot, ntail, iadn, iado, i, j, k
    integer :: l, nbligo, icompo, ldmat, ii, llplin, nbnoe, llplio, ldlid
    integer :: nblign, nbcoln, llmat, nbcolo, icompn, iblo
    parameter    (nbcmpm=10)
    integer :: ideco(nbcmpm), idecn(nbcmpm), icar(3)
    character(len=8) :: nomres, sst, intf, nommcl, basmod, nomg
    character(len=24) :: fmli, fplin, fplio, nomatn, famli
    real(kind=8) :: rot(3), matrot(nbcmpm, nbcmpm), fact, matbuf(nbcmpm, nbcmpm)
    real(kind=8) :: mattmp(nbcmpm, nbcmpm), zero, xo(nbcmpm), xn(nbcmpm)
!
!-----------------------------------------------------------------------
    data zero /0.0d+00/
!-----------------------------------------------------------------------
!
    call jemarq()
!
!-----RECUPERATION DU NOMBRE DU NOMBRE D'ENTIERS CODES ASSOCIE A DEPL_R
!
    nomg = 'DEPL_R'
    call dismoi('NB_EC', nomg, 'GRANDEUR', repi=nbec)
    if (nbec .gt. 10) then
        call utmess('F', 'MODELISA_94')
    endif
!
    ntail=nbcmpm**2
!
! --- RECUPERATION DES NOMS DU MACR-ELEMENT ET DE LA BASE MODALE DE SST
!
    call mgutdm(nomres, sst, ibid, 'NOM_MACR_ELEM', ibid,&
                nommcl)
    call mgutdm(nomres, sst, ibid, 'NOM_BASE_MODALE', ibid,&
                basmod)
!
! --- RECUPERATION DES ROTATIONS DE LA SOUS-STRUCTURE
!
    call jenonu(jexnom(nomres//'      .MODG.SSNO', sst), ibid)
    call jeveuo(jexnum(nomres//'      .MODG.SSOR', ibid), 'L', llrot)
    do 10 i = 1, 3
        rot(i)=zr(llrot+i-1)
 10 end do
!
! --- CALCUL DE LA MATRICE DE ROTATION
!
    call intet0(rot(1), mattmp, 3)
    call intet0(rot(2), matrot, 2)
    call r8inir(ntail, zero, matbuf, 1)
    call pmppr(mattmp, nbcmpm, nbcmpm, 1, matrot,&
               nbcmpm, nbcmpm, 1, matbuf, nbcmpm,&
               nbcmpm)
    call intet0(rot(3), mattmp, 1)
    call r8inir(ntail, zero, matrot, 1)
    call pmppr(matbuf, nbcmpm, nbcmpm, 1, mattmp,&
               nbcmpm, nbcmpm, 1, matrot, nbcmpm,&
               nbcmpm)
!
! --- RECUPERATION DES DONNEES MINI-PROFNO DE LA LIAISON NON ORIENTEE
!     ET ORIENTEE
!
    call jeveuo(jexnum(fplin, ii), 'L', llplin)
    call jelira(jexnum(fplin, ii), 'LONMAX', nbnoe)
    nbnoe=nbnoe/(1+nbec)
    call jeveuo(jexnum(fplio, ii), 'L', llplio)
!
! --- RECUPERATION DE LA MATRICE DE LIAISON ET ECRITURE DES DIMENSIONS
!     DE LA MATRICE ORIENTEE
!
    nomatn='&&ROTLIS.MAT.LIAN'
!
    famli=nomres//'      .MODG.LIDF'
    call jeveuo(jexnum(famli, ii), 'L', ldlid)
    if ((zk8(ldlid+2).eq.sst) .and. (zk8(ldlid+4).eq.'OUI     ')) then
        ordo=1
    else
        ordo=0
    endif
!
    call exmali(basmod, intf, ibid, nomatn, 'V',&
                nblign, nbcoln, ordo, ii)
    call jeveuo(nomatn, 'L', llmat)
!
! --- CREATION DE LA NOUVELLE MATRICE DE LIAISON
!
    nbligo=icar(1)
    nbcolo=icar(2)
    iblo=icar(3)
    call jecroc(jexnum(fmli, iblo))
    call jeecra(jexnum(fmli, iblo), 'LONMAX', nbcolo*nbligo)
    call jeveuo(jexnum(fmli, iblo), 'E', ldmat)
!
! --- CALCUL PROPREMENT DIT DES MATRICES ORIENTEES DE LIAISON----------
!
!  BOUCLE SUR LES NOEUDS DU PROFIL
!
    do 100 i = 1, nbnoe
        iadn=zi(llplin+(i-1)*(1+nbec))
        call isdeco(zi(llplin+(i-1)*(1+nbec)+1), idecn, nbcmpm)
        iado=zi(llplio+(i-1)*(1+nbec))
        call isdeco(zi(llplio+(i-1)*(1+nbec)+1), ideco, nbcmpm)
!
!  BOUCLE SUR LES DEFORMEES DE LA BASE
!
        do 110 j = 1, nbcoln
            icompo=iado-1
            icompn=iadn-1
!
!  BOUCLE SUR LES COMPOSANTES DE DEPART: NON ORIENTEES
!  INITIALISATION VALEURS
!
            do 120 k = 1, nbcmpm
                if (idecn(k) .gt. 0) then
                    icompn=icompn+1
                    xn(k)=zr(llmat+(j-1)*nblign+icompn-1)
                else
                    xn(k)=zero
                endif
120         continue
!
!  ROTATION DU DELACEMENT NODAL MODAL
!
            do 140 k = 1, nbcmpm
                xo(k)=zero
                do 150 l = 1, nbcmpm
                    xo(k)=xo(k)+matrot(k,l)*xn(l)
150             continue
140         continue
!
!  BOUCLE SUR LES COMPOSANTES ORIENTEES: RECUPERATION VALEURS
!
            do 130 k = 1, nbcmpm
                if (ideco(k) .gt. 0) then
                    icompo=icompo+1
                    zr(ldmat+(j-1)*nbligo+icompo-1)=xo(k)*fact
                endif
130         continue
110     continue
100 continue
!
    call jedetr(nomatn)
!
    call jedema()
end subroutine
