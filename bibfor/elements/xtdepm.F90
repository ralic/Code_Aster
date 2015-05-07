subroutine xtdepm(ndim, jnnm, jnne, ndeple, nsinge,&
                  nsingm, ffe, ffm, jdepde, rre,&
                  rrm, jddle, jddlm, nfhe, nfhm, lmulti,&
                  heavn, heavfa, ddeple, ddeplm)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/indent.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/vecini.h"
#include "asterfort/xcalc_code.h"
#include "asterfort/xcalc_heav.h"
    integer :: ndim, jnnm(3), jnne(3), nfhe, nfhm
    integer :: nsinge, nsingm, heavn(*), heavfa(*)
    real(kind=8) :: rre, rrm
    integer :: jdepde, ndeple, jddle(2), jddlm(2)
    real(kind=8) :: ffm(20), ffe(20)
    real(kind=8) :: ddeple(3), ddeplm(3)
    aster_logical :: lmulti
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEMGG - UTILITAIRE)
!
! CALCUL DES INCREMENTS - DEPLACEMENTS
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
! IN  NNES   : NOMBRE DE NOEUDS SOMMETS DE LA MAILLE ESCLAVE
! IN  NSINGE : NOMBRE DE FONCTIONS SINGULIERE ESCLAVES
! IN  NSINGM : NOMBRE DE FONCTIONS SINGULIERE MAIT RES
! IN  DDLES : NOMBRE DE DDLS D'UN NOEUD SOMMET ESCLAVE
! IN  RRE    : SQRT LSN PT ESCLAVE
! IN  RRM    : SQRT LSN PT MAITRE
! IN  JDEPDE : POINTEUR JEVEUX POUR DEPDEL
! IN  FFE    : FONCTIONS DE FORMES ESCLAVE
! IN  FFM    : FONCTIONS DE FORMES MAITRE
! OUT DDEPLE : INCREMENT DEPDEL DU DEPL. DU POINT DE CONTACT
! OUT DDEPLM : INCREMENT DEPDEL DU DEPL. DU PROJETE DU POINT DE CONTACT
!
!
!
!
    integer :: idim, inoe, inom, pl, in, iddl, hea_fa(2), nddle, nnem
    integer :: nne, nnes, nnm, nnms, ddles, ddlem, ddlms, ddlmm, ifh
    real(kind=8) :: iescl(6), imait(6)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nne=jnne(1)
    nnes=jnne(2)
    nnem=jnne(3)
    nnm=jnnm(1)
    nnms=jnnm(2)
    ddles=jddle(1)
    ddlem=jddle(2)
    ddlms=jddlm(1)
    ddlmm=jddlm(2)
    nddle=ddles*nnes+ddlem*nnem
!
    call vecini(3, 0.d0, ddeplm)
    call vecini(3, 0.d0, ddeple)
    call vecini(6,0.d0,iescl)
    call vecini(6,0.d0,imait)
!
    iescl(1) = 1
    iescl(2) = -1
    iescl(2+nfhe)=-rre
    imait(1) = 1
    imait(2) = 1
    imait(2+nfhm)= rrm
    if (.not.lmulti) then
      hea_fa(1)=xcalc_code(1,he_inte=[-1])
      hea_fa(2)=xcalc_code(1,he_inte=[+1])
    endif
!
!
    do 200 idim = 1, ndim
        do 210 inoe = 1, ndeple
            call indent(inoe, ddles, ddlem, nnes, in)
            if (nnm .ne. 0) then
                if (lmulti) then
                    do 30 ifh = 1, nfhe
                        iescl(1+ifh)=xcalc_heav(heavn(nfhe*(inoe-1)+ifh),&
                                                heavfa(1),&
                                                heavn(nfhe*nne+nfhm*nnm+inoe))
 30                 continue
                else
                        iescl(2)=xcalc_heav(heavn(inoe),&
                                            hea_fa(1),&
                                            heavn(nfhe*nne+nfhm*nnm+inoe))   
                endif
                do 40 iddl = 1, 1+nfhe
                    pl = in + (iddl-1)*ndim + idim
                    ddeple(idim) = ddeple(idim)+ ffe(inoe)*iescl(iddl)*zr(jdepde-1+ pl)
 40             continue
            endif
            do 215 iddl = 1+nfhe+1, 1+nfhe+nsinge
                pl = in + (iddl-1)*ndim + idim
                ddeple(idim) = ddeple(idim) +ffe(inoe)*iescl(2+nfhe)*zr(jdepde-1+pl)
215          continue
210      continue
200  end do
!
    do 201 idim = 1, ndim
        do 220 inom = 1, nnm
            call indent(inom, ddlms, ddlmm, nnms, in)
            in = in + nddle
            if (lmulti) then
                do 70 ifh = 1, nfhm
                    imait(1+ifh)=xcalc_heav(heavn(nfhe*nne+nfhm*(inom-1)+ifh),&
                                            heavfa(2),&
                                            heavn((1+nfhe)*nne+nfhm*nnm+inom))
 70             continue
            else
                    imait(2)=xcalc_heav(heavn(nne+inom),&
                                        hea_fa(2),&
                                        heavn((1+nfhe)*nne+nfhm*nnm+inom)) 
            endif
            do 80 iddl = 1, 1+nfhm+nsingm
                pl = in + (iddl-1)*ndim + idim
                ddeplm(idim) = ddeplm(idim) + ffm(inom)*imait(iddl)*zr(jdepde-1+pl)
 80         continue
220      continue
201  end do
!
    call jedema()
!
end subroutine
