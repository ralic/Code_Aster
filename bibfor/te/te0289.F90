subroutine te0289(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/nbsigm.h"
#include "asterfort/xselno.h"
#include "asterfort/xsseno.h"
#include "asterfort/xteini.h"
    character(len=16) :: option, nomte
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
!
!    - FONCTIONS REALISEES :  ROUTINE X-FEM
!
!          PASSAGE DES CONTRAINTES
!          DES POINTS DE GAUSS DES SOUS-ELEMENTS :
!            * AUX NOEUDS DES ELEMENTS PARENTS
!              (OPTION 'SIEF_ELNO' ET 'SIGM_ELNO') ;
!            * AUX SOMMETS (NOEUDS) DES SOUS-ELEMENTS
!              (OPTION 'SISE_ELNO') ;
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!
!
!
    integer :: mxval
    parameter (mxval=6*3*4)
!     EN 2D :
!     MXVAL =  6 (NBSE MAX) * 3 (NBNOSE MAX) * 4 (NBCMP MAX)
!     EN 3D :
!     MXVAL = 32 (NBSE MAX) * 4 (NBNOSE MAX) * 6 (NBCMP MAX)
!
    integer :: ibid, ndim, nnop, nno, npg, ivf, jgano
    integer :: nfh, nfe, singu, ddlc, nbsig
    integer :: jcnset, jlonch, jsigpg
    integer :: jout1, jout2
    integer :: nse
!
!
    real(kind=8) :: siseno(mxval)
!
    character(len=8) :: elrefp, elrese(3), fami(3)
!
    data    elrese /'SE2','TR3','TE4'/
    data    fami   /'BID','XINT','XINT'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
!-----------------------------------------------------------------------
!     INITIALISATIONS
!-----------------------------------------------------------------------
!
!     ELEMENT DE REFERENCE PARENT : RECUP DE NDIM ET NNOP
    call elref1(elrefp)
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nnop)
    ASSERT(nnop.le.27)
!
!
!     SOUS-ELEMENT DE REFERENCE : RECUP DE NNO, NPG, IVF ET JGANO
    call elrefe_info(elrefe=elrese(ndim),fami=fami(ndim),nno=nno,&
  npg=npg,jvf=ivf,jgano=jgano)
!
    ASSERT(npg.le.15)
!
!     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
    call xteini(nomte, nfh, nfe, singu, ddlc,&
                ibid, ibid, ibid, ibid, ibid,&
                ibid)
!
!     NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
    nbsig = nbsigm()
    ASSERT(nbsig.le.6)
!
!-----------------------------------------------------------------------
!     RECUPERATION DES ENTREES / SORTIE
!-----------------------------------------------------------------------
!
    call jevech('PLONCHA', 'L', jlonch)
    call jevech('PCONTRR', 'L', jsigpg)
!
!     RÉCUPÉRATION DE LA SUBDIVISION DE L'ÉLÉMENT EN NSE SOUS ELEMENT
    nse=zi(jlonch-1+1)
!
!-----------------------------------------------------------------------
!     OPTION SISE_ELNO
!-----------------------------------------------------------------------
!
    if (option .eq. 'SISE_ELNO') then
!
!       RECUPERATION DES ENTREES / SORTIE
        call jevech('PCONTSER', 'E', jout1)
!
!       CALCUL DES CONTRAINTES PAR SOUS-ELEMENTS AUX NOEUDS (SENO)
        call xsseno(nno, nbsig, nse, npg, jgano,&
                    jsigpg, zr(jout1))
!
!-----------------------------------------------------------------------
!     OPTION SIEF_ELNO ET SIGM_ELNO
!-----------------------------------------------------------------------
!
        else if ((option.eq.'SIEF_ELNO').or. (option.eq.'SIGM_ELNO'))&
    then
!
!       RECUPERATION DES ENTREES / SORTIE
        call jevech('PCNSETO', 'L', jcnset)
        call jevech('PSIEFNOR', 'E', jout2)
!
!       CALCUL DES CONTRAINTES PAR ELEMENTS AUX NOEUDS (ELNO)
        call xsseno(nno, nbsig, nse, npg, jgano,&
                    jsigpg, siseno)
        call xselno(nno, nnop, nbsig, nse, ndim,&
                    jcnset, siseno, jout2)
!
!-----------------------------------------------------------------------
!     OPTION NON PREVUE
!-----------------------------------------------------------------------
!
    else
!
        ASSERT(.false.)
!
    endif
!
    call jedema()
!
end subroutine
