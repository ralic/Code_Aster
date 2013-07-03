subroutine te0046(option, nomte)
    implicit   none
#include "jeveux.h"
#include "asterfort/elref1.h"
#include "asterfort/elref4.h"
#include "asterfort/iselli.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/reeref.h"
#include "asterfort/teattr.h"
#include "asterfort/vecini.h"
#include "asterfort/xteini.h"
    character(len=16) :: option, nomte
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
! person_in_charge: samuel.geniaut at edf.fr
!
!.......................................................................
!
!     BUT: CALCUL DES COORDONNEES DES POINTS DE GAUSS
!          DE LA FAMILLE X-FEM (POINTS DE GAUSS DES SOUS-ÉLÉMENTS)
!          DANS L'ESPACE DE L'ELEMENT PARENT DE REFERENCE
!
!          OPTIONS : 'XFEM_XPG'
!
!  ENTREES  ---> OPTION : OPTION DE CALCUL
!           ---> NOMTE  : NOM DU TYPE ELEMENT
!
!.......................................................................
!
!
!      CHARACTER*8   ELREF,FPG,ELC,NOMPAR(4)
!      INTEGER NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO
!      INTEGER NFH,NFE,SINGU,DDLC,DDLS,NDDL
!      INTEGER IGEOM,IPRES,ITEMPS,IFORC,IRET,IRES
!      INTEGER JLST,JPTINT,JAINT,JCFACE,JLONCH
!      INTEGER I,J,NINTER,NFACE,CFACE(5,3),IFA,NLI,IN(3),IG
!      INTEGER AR(12,3),NBAR,FAC(6,4),NBF,IBID2(12,3),IBID,INO,ILEV
!      INTEGER NNOF,NPGF,IPOIDF,IVFF,IDFDEF,IPGF,POS
!      REAL*8  MULT,PRES,CISA, FORREP(3,2),FF(27),G(3),JAC,ND(3),HE(2)
!      REAL*8  RR(2),LST,XG(4)
!      DATA    HE / -1.D0 , 1.D0/
!
    character(len=8) :: elrefp, elrese(6), fami(6), enr
    real(kind=8) :: xg(3), xe(3), ff(27), rbid, coorse(81)
    integer :: ibid, ndim, nnop, nno, npg, ivf
    integer :: nfh, nfe, singu, ddlc, jpmilt, irese
    integer :: jpintt, jcnset, jheavt, jlonch, igeom, jout
    integer :: i, j, nse, ise, in, ino, ipg, kpg
    logical :: axi
!
    data    elrese /'SE2','TR3','TE4','SE3','TR6','TE4'/
    data    fami   /'BID','XINT','XINT','BID','XINT','XINT'/
!
    call jemarq()
!
!-----------------------------------------------------------------------
!     INITIALISATIONS
!-----------------------------------------------------------------------
!
!     ELEMENT DE REFERENCE PARENT : RECUP DE NDIM ET NNOP
    call elref1(elrefp)
    call elref4(' ', 'RIGI', ndim, nnop, ibid,&
                ibid, ibid, ibid, ibid, ibid)
!
    axi = lteatt(' ','AXIS','OUI')
!
!     SOUS-ELEMENT DE REFERENCE : RECUP DE NNO, NPG ET IVF
    if (.not.iselli(elrefp) .and. ndim .le. 2) then
        irese=3
    else
        irese=0
    endif
    call elref4(elrese(ndim+irese), fami(ndim+irese), ibid, nno, ibid,&
                npg, ibid, ivf, ibid, ibid)
!
!     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
    call xteini(nomte, nfh, nfe, singu, ddlc,&
                ibid, ibid, ibid, ibid, ibid,&
                ibid)
!
!-----------------------------------------------------------------------
!     RECUPERATION DES ENTREES / SORTIE
!-----------------------------------------------------------------------
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PPINTTO', 'L', jpintt)
    call jevech('PCNSETO', 'L', jcnset)
    call jevech('PHEAVTO', 'L', jheavt)
    call jevech('PLONCHA', 'L', jlonch)
!     PROPRES AUX ELEMENTS 1D ET 2D (QUADRATIQUES)
    call teattr(nomte, 'S', 'XFEM', enr, ibid)
    if ((ibid.eq.0) .and. (nomte(3:4).ne.'AX') .and.&
        (enr.eq.'XH' .or.enr.eq.'XHT'.or.enr.eq.'XT'.or.enr.eq.'XHC') .and. ndim .le. 2) &
    call jevech('PPMILTO', 'L', jpmilt)
!
    call jevech('PXFGEOM', 'E', jout)
!
!     RÉCUPÉRATION DE LA SUBDIVISION DE L'ÉLÉMENT EN NSE SOUS ELEMENT
    nse=zi(jlonch-1+1)
!
!       BOUCLE D'INTEGRATION SUR LES NSE SOUS-ELEMENTS
    do 110 ise = 1, nse
!
!       BOUCLE SUR LES SOMMETS DU SOUS-TRIA (DU SOUS-SEG)
        do 111 in = 1, nno
            ino=zi(jcnset-1+nno*(ise-1)+in)
            do 112 j = 1, ndim
                if (ino .lt. 1000) then
                    coorse(ndim*(in-1)+j)=zr(igeom-1+ndim*(ino-1)+j)
                else if (ino.gt.1000 .and. ino.lt.2000) then
                    coorse(ndim*(in-1)+j)=zr(jpintt-1+ndim*(ino-1000-&
                    1)+j)
                else if (ino.gt.2000 .and. ino.lt.3000) then
                    coorse(ndim*(in-1)+j)=zr(jpmilt-1+ndim*(ino-2000-&
                    1)+j)
                else if (ino.gt.3000) then
                    coorse(ndim*(in-1)+j)=zr(jpmilt-1+ndim*(ino-3000-&
                    1)+j)
                endif
112          continue
111      continue
!
!
!-----------------------------------------------------------------------
!         BOUCLE SUR LES POINTS DE GAUSS DU SOUS-ELT
!-----------------------------------------------------------------------
!
        do 200 kpg = 1, npg
!
!         COORDONNÉES DU PT DE GAUSS DANS LE REPÈRE RÉEL : XG
            call vecini(ndim, 0.d0, xg)
            do 210 i = 1, ndim
                do 211 in = 1, nno
                    xg(i) = xg(i) + zr(ivf-1+nno*(kpg-1)+in) * coorse( ndim*(in-1)+i)
211              continue
210          continue
!
!         COORDONNEES DU PG DANS L'ELEMENT DE REF PARENT : XE
            call reeref(elrefp, axi, nnop, ibid, zr(igeom),&
                        xg, 1, .false., ndim, rbid,&
                        rbid, rbid, ibid, ibid, ibid,&
                        ibid, ibid, ibid, rbid, rbid,&
                        'NON', xe, ff, rbid, rbid,&
                        rbid, rbid)
!
!         NUMERO DE CE POINT DE GAUSS DANS LA FAMILLE 'XFEM'
            ipg= (ise-1) * npg + kpg
!
            do 220 j = 1, ndim
                zr(jout-1+ndim*(ipg-1)+j) = xe(j)
220          continue
!
!
200      continue
!
!-----------------------------------------------------------------------
!         FIN DE LA BOUCLE SUR LES POINTS DE GAUSS DU SOUS-ELT
!-----------------------------------------------------------------------
!
!
110  end do
!
!
    call jedema()
end subroutine
