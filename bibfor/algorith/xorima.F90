subroutine xorima(noma, nbmaf, jdlima, jconx1, jconx2,&
                  jcoor, sens)
!
    implicit none
!
#include "jeveux.h"
!
#include "asterfort/cncinv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jerazo.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
#include "blas/ddot.h"
    character(len=8) :: noma
    character(len=19) :: sens
    integer :: nbmaf, jdlima, jconx1, jconx2, jcoor
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
! person_in_charge: daniele.colombo at ifpen.fr
!
!  ---------------------------------------------------------------------
!
!       XORIMA   : X-FEM ORIENTATION MAILLES : ORIENTATION DE LA NORMALE
!       ------     -     ---         --
!                  DES MAILLES DEFINISSANT LA SURFACE DE LA FISSURE
!
!  DANS LE CADRE DE LA DEFINITION X-FEM D'UNE FISSURE 3D PAR UN MAILLAGE
!  SURFACIQUE, LA NORMALE DE CHAQUE ELEMENT DE CE MAILLAGE DOIT ETRE
!  BIEN ORIENTEE PAR RAPPORT A CELLES DES AUTRES MAILLES AFIN DE BIEN
!  CALCULER LA SIGNE DE LA LEVEL SET NORMALE.
!
!  ENTREE
!  ------
!
!     NOMA   = NOM DU MAILLAGE SURFACIQUE DEFINISSANT LA FISSURE
!     NBMAF  = NOMBRE DE MAILLES DANS JDLIMA
!     JDLIMA = POINTER A L'OBJET JEVEUX QUI CONTIENT LA LISTE DES
!              MAILLES DE NOMA QUI FORMENT LA SURFACE DE LA FISSURE
!     JCONX1 = POINTER A L'OBJET JEVEUX NOMA//'.CONNEX'
!     JCONX2 = POINTER A L'OBJET JEVEUX LONGUEUR CUMULEE DE JCONX1
!     JCOOR  = POINTER A L'OBJET JEVEUX DES COORDONNES DES NOEUDS DU
!              MAILLAGE NOMA
!     SENS   = NOM DE L'OBJET JEVEUX A CREER (VOIR CI-DESSOUS)
!
!  SORTIE
!  ------
!
!     SENS   = OBJET JEVEUX REMPLI. IL S'AGIT D'UN VECTEUR D'ENTIERS DE
!              LONGUEUR NBMAF CONTENANT SEULEMENT DES 1 OU DES -1.
!              PENDANT LE CALCUL DE LA LEVEL SET NORMALE PAR PROJECTION
!              SUR LES ELEMENT DE JDLIMA, LA NORMALE A L'ELEMENT DOIT
!              ETRE MULTIPLIEE PAR LA VALEUR CONTENUE DANS SENS AFIN DE
!              REORIENTER LA NORMALE (-1) SI NECESSAIRE POUR BIEN
!              CALCULER LA SIGNE DE LA LEVEL SET NORMALE.
!
!
    real(kind=8) :: a(3), b(3), c(3), ab(3), ac(3), vn(3), vnref(3), ps, norme
    integer :: jsens, nlayer, layer, i, j, nmaabs, nbnoma, inoma, jelno, nbelno
    integer :: elj, numelm, nmaass
    character(len=19) :: cnxinv
    character(len=8) :: k8b
!
!-----------------------------------------------------------------------
    integer :: nuno
!-----------------------------------------------------------------------
    call jemarq()
!
!     CREATE THE VECTOR WHERE THE ORIENTATION OF THE ELEMENT NORMAL WILL
!     BE STORED
    call wkvect(sens, 'V V I', nbmaf, jsens)
!
!     CREATE A TEMPORARY VECTOR WHERE THE LAYER TO WHICH EACH ELEMENT
!     BELONGS IS STORED
    call wkvect('&&XORIMA.LAY', 'V V I', nbmaf, nlayer)
    call jerazo('&&XORIMA.LAY', nbmaf, 1)
    call jeveuo('&&XORIMA.LAY', 'E', nlayer)
!
!     THE FIRST ELEMENT IS TAKEN AS THE REFERENCE
    zi(nlayer-1+1) = 1
    zi(jsens-1+1) = 1
!
!     CREATE THE INVERSE CONNECTIVITY OBJECT
    cnxinv = '&&XORIMA.CNCINV'
    call cncinv(noma, zi(jdlima), nbmaf, 'V', cnxinv)
!
!     LOOP ON THE LAYER NUMBER
    do 10 layer = 1, nbmaf
!
!        LOOP ON THE ELEMENT LIST
        do 20 i = 1, nbmaf
!
!           SEARCH FOR THE ELEMENTS BELONGING TO THE CURRENT LAYER
            if (zi(nlayer-1+i) .eq. layer) then
!
!              CALCULATE THE NORMAL TO THE ELEMENT. IT WILL BE USED
!              AS A REFERENCE FOR THE CONNECTED ELEMENTS.
                nmaabs = zi(jdlima-1+i)
                nbnoma = zi(jconx2+nmaabs)-zi(jconx2+nmaabs-1)
!
                inoma=1
                nuno=zi(jconx1-1+zi(jconx2+nmaabs-1)+inoma-1)
                a(1)=zr(jcoor-1+3*(nuno-1)+1)
                a(2)=zr(jcoor-1+3*(nuno-1)+2)
                a(3)=zr(jcoor-1+3*(nuno-1)+3)
!
                inoma=2
                nuno=zi(jconx1-1+zi(jconx2+nmaabs-1)+inoma-1)
                b(1)=zr(jcoor-1+3*(nuno-1)+1)
                b(2)=zr(jcoor-1+3*(nuno-1)+2)
                b(3)=zr(jcoor-1+3*(nuno-1)+3)
!
                inoma=3
                nuno=zi(jconx1-1+zi(jconx2+nmaabs-1)+inoma-1)
                c(1)=zr(jcoor-1+3*(nuno-1)+1)
                c(2)=zr(jcoor-1+3*(nuno-1)+2)
                c(3)=zr(jcoor-1+3*(nuno-1)+3)
!
                ab(1)=b(1)-a(1)
                ab(2)=b(2)-a(2)
                ab(3)=b(3)-a(3)
!
                ac(1)=c(1)-a(1)
                ac(2)=c(2)-a(2)
                ac(3)=c(3)-a(3)
!
                call provec(ab, ac, vnref)
                call normev(vnref, norme)
!
                vnref(1) = vnref(1)*zi(jsens-1+i)
                vnref(2) = vnref(2)*zi(jsens-1+i)
                vnref(3) = vnref(3)*zi(jsens-1+i)
!
!              LOOP ON EACH NODE OF THE SELECTED ELEMENT
                do 30 j = 1, nbnoma
!
                    nuno=zi(jconx1-1+zi(jconx2+nmaabs-1)+j-1)
!
!                 SEARCH FOR THE ELEMENTS CONNECTED TO THE SELECTED ONE
!                 BY MEANS OF NODE J
                    call jelira(jexnum(cnxinv, nuno), 'LONMAX', nbelno, k8b)
                    call jeveuo(jexnum(cnxinv, nuno), 'L', jelno)
!
!                 LOOP ON EACH ELEMENT CONNECTED TO NODE J
                    do 40 elj = 1, nbelno
!
                        numelm=zi(jelno-1+elj)
!
!                    CHECK IF THE CONNECTED ELEMENT HAS ALREADY BEEN
!                    SELECTED
                        if (zi(nlayer-1+numelm) .eq. 0) then
!
!                       NO. ASSIGN IT THE LAYER NUMBER
                            zi(nlayer-1+numelm) = layer+1
!
!                       CALCULATE THE NORMAL TO THE ELEMENT USING THE
!                       FIRST THREE NODES DEFINING IT
                            nmaass = zi(jdlima-1+numelm)
                            nbnoma = zi(jconx2+nmaass)-zi(jconx2+ nmaass-1)
!
                            inoma=1
                            nuno=zi(jconx1-1+zi(jconx2+nmaass-1)+&
                            inoma-1)
                            a(1)=zr(jcoor-1+3*(nuno-1)+1)
                            a(2)=zr(jcoor-1+3*(nuno-1)+2)
                            a(3)=zr(jcoor-1+3*(nuno-1)+3)
!
                            inoma=2
                            nuno=zi(jconx1-1+zi(jconx2+nmaass-1)+&
                            inoma-1)
                            b(1)=zr(jcoor-1+3*(nuno-1)+1)
                            b(2)=zr(jcoor-1+3*(nuno-1)+2)
                            b(3)=zr(jcoor-1+3*(nuno-1)+3)
!
                            inoma=3
                            nuno=zi(jconx1-1+zi(jconx2+nmaass-1)+&
                            inoma-1)
                            c(1)=zr(jcoor-1+3*(nuno-1)+1)
                            c(2)=zr(jcoor-1+3*(nuno-1)+2)
                            c(3)=zr(jcoor-1+3*(nuno-1)+3)
!
                            ab(1)=b(1)-a(1)
                            ab(2)=b(2)-a(2)
                            ab(3)=b(3)-a(3)
!
                            ac(1)=c(1)-a(1)
                            ac(2)=c(2)-a(2)
                            ac(3)=c(3)-a(3)
!
                            call provec(ab, ac, vn)
                            call normev(vn, norme)
!
!                       CHECK THE ORIENTATION OF THE ELEMENT NORMAL
!                       WITH RESPECT TO THE REFERENCE NORMAL
                            ps=ddot(3,vn,1,vnref,1)
!
                            if (ps .lt. 0.d0) then
                                zi(jsens-1+numelm)=-1
                            else
                                zi(jsens-1+numelm)=1
                            endif
!
                        endif
!
40                  continue
!
30              continue
!
            endif
!
20      continue
!
10  end do
!
!     LOOP ON THE ELEMENT LIST TO CHECK THAT EACH ELEMENT HAS BEEN
!     PROCESSED
    do 100 i = 1, nbmaf
        if (zi(nlayer-1+i) .eq. 0) call u2mess('F', 'XFEM_9')
100  end do
!
!     CLEAN THE TEMPORARY JEVEUX OBJECTS
    call jedetr('&&XORIMA.LAY')
    call jedetr(cnxinv)
!
    call jedema()
end subroutine
