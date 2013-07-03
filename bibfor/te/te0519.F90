subroutine te0519(option, nomte)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit   none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/elref1.h"
#include "asterfort/elref4.h"
#include "asterfort/elrfvf.h"
#include "asterfort/indent.h"
#include "asterfort/iselli.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/tecach.h"
#include "asterfort/vecini.h"
#include "asterfort/xteini.h"
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
!  XFEM GRANDS GLISSEMENTS
!  REACTUALISATION DES GEOMETRIES DES FACETTES DE CONTACT
!  (MAITRE ET ESCLAVE)
!
!  OPTION : 'GEOM_FAC' (X-FEM GEOMETRIE DES FACETTES DE CONTACT)
!
!  ENTREES  ---> OPTION : OPTION DE CALCUL
!           ---> NOMTE  : NOM DU TYPE ELEMENT
!
!
!
!
    character(len=8) :: elref
    integer :: jdepl, jpint, jlon, jgeo, jlst, nfiss
    integer :: jges, jgma, ifiss, ifh, ncompp, jtab(2), iret, ncomph
    integer :: ibid, ndim, nno, nfh, singu, ddls, ninter
    integer :: i, j, ipt, nfe, ddlc, nnom, nddl, ddlm, nnos, in
    integer :: jfisno, jheafa, jeu(2)
    real(kind=8) :: ptref(3), deple(3), deplm(3), ff(20), lst, r
!
! ---------------------------------------------------------------------
!
    call assert(option.eq.'GEOM_FAC')
!
    call jemarq()
!
! --- RECUPERATION DU TYPE DE MAILLE, DE SA DIMENSION
! --- ET DE SON NOMBRE DE NOEUDS
!
    call elref1(elref)
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                ibid, ibid, ibid, ibid, ibid)
!
! --- INITIALISATION DES DIMENSIONS DES DDLS X-FEM
    call xteini(nomte, nfh, nfe, singu, ddlc,&
                nnom, ddls, nddl, ddlm, nfiss,&
                ibid)
!
! --- RECUPERATION DES ENTRÉES / SORTIE
!
    call jevech('PDEPLA', 'L', jdepl)
    call jevech('PPINTER', 'L', jpint)
    call jevech('PLONCHA', 'L', jlon)
    call jevech('PLST', 'L', jlst)
! --- LES GEOMETRIES MAITRES ET ESCLAVES INITIALES SONT
! --- ET RESTENT LES MEMES
    call jevech('PGESCLO', 'L', jgeo)
! --- CAS MULTI-HEAVISIDE
    if (nfiss .gt. 1) then
        call jevech('PFISNO', 'L', jfisno)
        call jevech('PHEAVFA', 'L', jheafa)
        call tecach('OOO', 'PHEAVFA', 'L', 2, jtab,&
                    iret)
        ncomph = jtab(2)
    endif
    call tecach('OOO', 'PPINTER', 'L', 2, jtab,&
                iret)
    ncompp = jtab(2)
    jeu(1) = -1
    jeu(2) = +1
!
    call jevech('PNEWGES', 'E', jges)
    call jevech('PNEWGEM', 'E', jgma)
!
! --- BOUCLE SUR LES FISSURES
!
    do 10 ifiss = 1, nfiss
!
! --- RECUPERATION DU NOMBRE DES POINTS D'INTERSECTION
!
        ninter=zi(jlon-1+3*(ifiss-1)+1)
        if (ndim .eq. 2 .and. .not.iselli(elref)) then
            if (ninter .eq. 2) ninter=3
        endif
!
! --- BOUCLE SUR LES POINTS D'INTERSECTION
!
        do 100 ipt = 1, ninter
            call vecini(ndim, 0.d0, deple)
            call vecini(ndim, 0.d0, deplm)
            do 110 i = 1, ndim
!
! --- RECUPERATION DES COORDONNEES DE REFERENCE DU POINT D'INTERSECTION
!
                ptref(i)=zr(jpint-1+ncompp*(ifiss-1)+ndim*(ipt-1)+i)
110          continue
!
! --- CALCUL DES FONCTIONS DE FORMES DU POINT D'INTERSECTION
!
            call elrfvf(elref, ptref, nno, ff, nno)
!
            if (singu .eq. 1) then
! --- CALCUL DE RR = SQRT(DISTANCE AU FOND DE FISSURE)
                lst=0.d0
                do 200 i = 1, nno
                    lst=lst+zr(jlst-1+i)*ff(i)
200              continue
                r=sqrt(abs(lst))
            endif
!
! --- CALCUL DES DEPLACEMENTS MAITRES ET ESCLAVES
! --- DU POINT D'INTERSECTION
!
            do 210 i = 1, nno
                call indent(i, ddls, ddlm, nnos, in)
                do 220 j = 1, ndim
                    deplm(j)=deplm(j)+ff(i)*zr(jdepl-1+in+j)
                    deple(j)=deple(j)+ff(i)*zr(jdepl-1+in+j)
220              continue
                do 230 ifh = 1, nfh
                    if (nfiss .gt. 1) then
                        jeu(1) = zi(&
                                 jheafa-1+ncomph*(nfiss*(ifiss-1) + zi(jfisno-1+nfh*(i-1)+ifh)-1&
                                 )+1&
                                 )
                        jeu(2) = zi(&
                                 jheafa-1+ncomph*(nfiss*(ifiss-1) + zi(jfisno-1+nfh*(i-1)+ifh)-1&
                                 )+2&
                                 )
                    endif
                    do 250 j = 1, ndim
                        deplm(j)=deplm(j)+jeu(2)*ff(i)*zr(jdepl-1+in+&
                        ndim*ifh+j)
                        deple(j)=deple(j)+jeu(1)*ff(i)*zr(jdepl-1+in+&
                        ndim*ifh+j)
250                  continue
230              continue
                do 240 j = 1, singu*ndim
                    deplm(j)=deplm(j)+r*ff(i)*zr(jdepl-1+in+ndim*(1+&
                    nfh)+j)
                    deple(j)=deple(j)-r*ff(i)*zr(jdepl-1+in+ndim*(1+&
                    nfh)+j)
240              continue
210          continue
!
! --- CALCUL DES NOUVELLES COORDONNEES DES POINTS D'INTERSECTIONS
! --- MAITRES ET ESCLAVES, ON FAIT :
! --- NOUVELLES COORDONNEES = ANCIENNES COORDONEES + DEPLACEMENT
!
            do 300 i = 1, ndim
                zr(jges-1+ncompp*(ifiss-1)+ndim*(ipt-1)+i) = zr(&
                                                             jgeo- 1+ncompp*(ifiss-1)+ndim*(ipt-1&
                                                             &)+i) + deple(i&
                                                             )
                zr(jgma-1+ncompp*(ifiss-1)+ndim*(ipt-1)+i) = zr(&
                                                             jgeo- 1+ncompp*(ifiss-1)+ndim*(ipt-1&
                                                             &)+i) + deplm(i&
                                                             )
300          continue
100      continue
!
10  end do
!
    call jedema()
end subroutine
