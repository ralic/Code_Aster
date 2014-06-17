subroutine singum(nomail, ndim, nnoem, nelem, itype,&
                  xy)
! aslint: disable=W1306
    implicit none
#include "jeveux.h"
#include "asterfort/cncinv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/nomil.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: ndim, nnoem, nelem, itype(nelem)
    real(kind=8) :: xy(3, nnoem)
    character(len=8) :: nomail
! ----------------------------------------------------------------------
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
!     BUT:
!         CREATION D'OBJETS TEMPORAIRES NECESSAIRES POUR LE CALCUL
!         OPTION : 'SING_ELEM'
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   NOMAIL       : NOM UTILISATEUR DU MAILLAGE
! IN   NDIM     : DIMENSION DU PROBLEME
! IN   NNOEM        : NOMBRE DE NOEUDS DU MAILLAGE
! IN   NELEM        : NOMBRE D ELEMENTS FINIS DU MAILLAGE
! IN   ITYPE(NELEM) : NUMERO DU TYPE D'ELEMENT FINI
! IN   XY(3,NNOEM)  : XYONNEES DES NOEUDS
!
!      SORTIE :
!-------------
! OUT STOCKES DANS DES OBJETS TEMPORAIRES '&&SINGUM.XXXX'
! 1) '&&SINGUM.DIME' (DIM=3) CONTIENT
!     NBRE MAX DE NOEUDS SOMMETS CONNECTES AUX EFS UTILES (NSOMMX)
!     NBRE MAX D EFS UTILES CONNECTES AUX NOEUDS SOMMETS (NELCOM)
!     EN 2D UTILE = QUAD OU TRIA
!     EN 3D UTILE = TETRA OU HEXA
!     ORDRE DES EF (1 SI LINEAIRE ET 2 SI QUADRATIQUE)
! 2) '&&SINGUM.MESU' (DIM=NELEM) CONTIENT L AIRE OU LE VOLUME DES EFS
! 3) '&&SINGUM.CONN' (DIM=NELEM*(NSOMMX+2)) CONTIENT
!     1ERE VALEUR = NBRE DE NOEUDS SOMMETS CONNECTES A L EF N°X
!     2EME VALEUR = 1 SI EF UTILE 0 SINON
!     CONNECTIVITE  EF N°X=>N° DE NOEUDS SOMMETS CONNECTES A X
! 4) '&&SINGUM.CINV' (DIM=NNOEM*(NELCOM+2)) CONTIENT
!     1ERE VALEUR = NBRE D EFS UTILES CONNECTES AU NOEUD N°X
!     2EME VALEUR = 0 NOEUD MILIEU OU NON CONNECTE A UN EF UTILE
!                   1 NOEUD SOMMET A L INTERIEUR + LIE A UN EF UTILE
!                   2 NOEUD SOMMET BORD + LIE A UN EF UTILE
!     CONNECTIVITE  NOEUD N°X=>N° DES EF UTILE CONNECTES A X
!
! ......................................................................
!
!
!
!
    integer ::  jconn2, jcinv1, jcinv2
    integer :: jdime, jmesu, jconn, jcinv, adress
    integer :: inno, inel, jel, nuef, nuno, i
    integer :: ifac, isur, ia
    integer :: nfac, nsur, nbpt, nbar
    integer :: nsommx, nelcom, nbre, nbef(nnoem)
    integer :: ordre
    integer :: n1, n2, n3, n4, n5, n6, n7, n8
    integer :: pt1(24), pt2(24), nm(12)
    integer :: nomili(nnoem)
    character(len=8) :: typema(nelem), typma2
    character(len=24) :: cinv
    character(len=24) :: chdime, chmesu, chconn, chcinv
    real(kind=8) :: aire, volume
    logical :: test, complet
    integer, pointer :: connex(:) => null()
!
    call jemarq()
!
! 1 - ADRESSES DE CONNECTIVITE EF=>NOEUDS CONNECTES
!                           ET NOEUD=>EF CONNECTES
!
    call jeveuo(nomail//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(nomail//'.CONNEX', 'LONCUM'), 'L', jconn2)
!
    cinv = '&&SINGU.CONNECINVERSE   '
    call cncinv(nomail, [0], 0, 'V', cinv)
    call jeveuo(cinv, 'L', jcinv1)
    call jeveuo(jexatr(cinv, 'LONCUM'), 'L', jcinv2)
!
! 2 - INITIALISATION DE NOMILI(NNOEM)
!     TOUS LES NOEUDS SONT SOMMETS A L INTERIEUR
!
    do 10 inno = 1, nnoem
        nomili(inno)=1
10  end do
!
! 3 - ON REMPLIT LES OBJETS '&&SINGUM.CONN' ET '&&SINGUM.MESU'
!     '&&SINGUE.CONN' (DIM=NELEM*(NSOMMX+2)) CONTIENT
!       1ERE VALEUR = NBRE DE NOEUDS SOMMETS CONNECTES A L EF N°X
!       2EME VALEUR = 1 SI EF UTILE 0 SINON
!       CONNECTIVITE  EF N°X=>N° DE NOEUDS SOMMETS CONNECTES A X
!    '&&SINGUM.MESU' : AIRE OU VOLUME DES EFS
!    ORDRE : ORDRE DES EF
!    DETECTION DES NOEUDS MILIEUX + NOEUDS BORDS
!
    nsommx=4
    chmesu='&&SINGUM.MESU           '
    call wkvect(chmesu, 'V V R', nelem, jmesu)
    chconn='&&SINGUM.CONN           '
    call wkvect(chconn, 'V V I', nelem*(nsommx+2), jconn)
!
    ordre=0
    typma2=' '
    complet=.false.
    do 20 inel = 1, nelem
        call jenuno(jexnum('&CATA.TM.NOMTM', itype(inel)), typema(inel))
        if (typema(inel)(1:4) .eq. 'HEXA' .or. typema(inel)(1:5) .eq. 'PENTA' .or.&
            typema(inel)(1:5) .eq. 'PYRAM') then
            call utmess('F', 'CALCULEL3_98')
        endif
!
! OBJETS '&&SINGUM.CONN'
!
        adress=jconn+(nsommx+2)*(inel-1)
        if (typema(inel)(1:4) .eq. 'POI1') zi(adress+1-1)=1
        if (typema(inel)(1:3) .eq. 'SEG') zi(adress+1-1)=2
        if (typema(inel)(1:4) .eq. 'TRIA') zi(adress+1-1)=3
        if (typema(inel)(1:4) .eq. 'QUAD') zi(adress+1-1)=4
        if (typema(inel)(1:5) .eq. 'TETRA') zi(adress+1-1)=4
        if (typema(inel)(1:4) .eq. 'HEXA') zi(adress+1-1)=8
        zi(adress+2-1)=0
        if (ndim .eq. 2) then
            if (typema(inel)(1:4) .eq. 'TRIA' .or. typema(inel)(1:4) .eq. 'QUAD') then
                zi(adress+2-1)=1
            endif
        else
            if (typema(inel)(1:5) .eq. 'TETRA' .or. typema(inel)(1:4) .eq. 'HEXA') then
                zi(adress+2-1)=1
            endif
        endif
        do 30 inno = 1, zi(adress+1-1)
            nuno=connex(zi(jconn2+inel-1)+inno-1)
            zi(adress+inno+2-1)=nuno
30      continue
!
! OBJET '&&SINGUM.MESU' + RECHERCHE NOEUD BORD
!
        zr(jmesu+inel-1)=0.d0
        if (ndim .eq. 2) then
            if (typema(inel)(1:4) .eq. 'TRIA') then
                n1=connex(zi(jconn2+inel-1)+1-1)
                n2=connex(zi(jconn2+inel-1)+2-1)
                n3=connex(zi(jconn2+inel-1)+3-1)
                aire=(xy(1,n2)-xy(1,n1))*(xy(2,n3)-xy(2,n1)) -(xy(2,&
                n2)-xy(2,n1))*(xy(1,n3)-xy(1,n1))
                zr(jmesu+inel-1)=abs(aire)/2.d0
                nfac=3
                pt1(1)=n1
                pt1(2)=n2
                pt1(3)=n3
                pt1(4)=n1
            endif
            if (typema(inel)(1:4) .eq. 'QUAD') then
                n1=connex(zi(jconn2+inel-1)+1-1)
                n2=connex(zi(jconn2+inel-1)+2-1)
                n3=connex(zi(jconn2+inel-1)+3-1)
                n4=connex(zi(jconn2+inel-1)+4-1)
                aire =((xy(1,n2)-xy(1,n1))*(xy(2,n4)-xy(2,n1))-&
                (xy(2,n2)-xy(2,n1))*(xy(1,n4)-xy(1,n1))) +((xy(1,n4)-&
                xy(1,n3))*(xy(2,n2)-xy(2,n3))- (xy(2,n4)-xy(2,n3))*(&
                xy(1,n2)-xy(1,n3)))
                zr(jmesu+inel-1)=abs(aire)/2.d0
                nfac=4
                pt1(1)=n1
                pt1(2)=n2
                pt1(3)=n3
                pt1(4)=n4
                pt1(5)=n1
            endif
        else
            if (typema(inel)(1:5) .eq. 'TETRA') then
                n1=connex(zi(jconn2+inel-1)+1-1)
                n2=connex(zi(jconn2+inel-1)+2-1)
                n3=connex(zi(jconn2+inel-1)+3-1)
                n4=connex(zi(jconn2+inel-1)+4-1)
                volume =(xy(1,n1)-xy(1,n2))* ((xy(2,n1)-xy(2,n3))*(xy(&
                3,n1)-xy(3,n4)) -(xy(2,n1)-xy(2,n4))*(xy(3,n1)-xy(3,&
                n3)))
                volume = volume-(&
                         xy(1,n1)-xy(1,n3))* ((xy(2,n1)-xy(2, n2))*(xy(3,n1)-xy(3,n4)) -(xy(2,n1)&
                         &-xy(2,n4))*(xy(3, n1)-xy(3,n2))&
                         )
                volume = volume+(&
                         xy(1,n1)-xy(1,n4))* ((xy(2,n1)-xy(2, n2))*(xy(3,n1)-xy(3,n3)) - (xy(2,n1&
                         &)-xy(2,n3))*(xy(3, n1)-xy(3,n2))&
                         )
                zr(jmesu+inel-1)=abs(volume) / 6.d0
                nfac=4
                nbpt=3
                pt1(1)=n1
                pt1(2)=n2
                pt1(3)=n3
                pt1(4)=n1
                pt1(5)=n3
                pt1(6)=n4
                pt1(7)=n2
                pt1(8)=n3
                pt1(9)=n4
                pt1(10)=n1
                pt1(11)=n2
                pt1(12)=n4
            endif
            if (typema(inel)(1:4) .eq. 'HEXA') then
                n1=connex(zi(jconn2+inel-1)+1-1)
                n2=connex(zi(jconn2+inel-1)+2-1)
                n3=connex(zi(jconn2+inel-1)+3-1)
                n4=connex(zi(jconn2+inel-1)+4-1)
                n5=connex(zi(jconn2+inel-1)+5-1)
                n6=connex(zi(jconn2+inel-1)+6-1)
                n7=connex(zi(jconn2+inel-1)+7-1)
                n8=connex(zi(jconn2+inel-1)+8-1)
                nfac=6
                nbpt=4
                pt1(1)=n1
                pt1(2)=n2
                pt1(3)=n3
                pt1(4)=n4
                pt1(5)=n5
                pt1(6)=n6
                pt1(7)=n7
                pt1(8)=n8
                pt1(9)=n1
                pt1(10)=n2
                pt1(11)=n6
                pt1(12)=n5
                pt1(13)=n4
                pt1(14)=n3
                pt1(15)=n7
                pt1(16)=n8
                pt1(17)=n2
                pt1(18)=n3
                pt1(19)=n7
                pt1(20)=n6
                pt1(21)=n1
                pt1(22)=n4
                pt1(23)=n8
                pt1(24)=n5
            endif
        endif
!
        if (zi(adress+2-1) .ne. 1) goto 70
        do 40 ifac = 1, nfac
            if (ndim .eq. 2) then
                n1=pt1(ifac)
                n2=pt1(ifac+1)
            else
                n1=pt1(nbpt*(ifac-1)+1)
                n2=pt1(nbpt*(ifac-1)+2)
                n3=pt1(nbpt*(ifac-1)+3)
                if (typema(inel)(1:4) .eq. 'HEXA') n4=pt1(nbpt*(ifac-1)+ 4)
            endif
            do 50 jel = 1, nelem
                if (jel .eq. inel) goto 50
                call jenuno(jexnum('&CATA.TM.NOMTM', itype(jel)), typema(jel))
                if (ndim .eq. 2) then
                    if (typema(jel)(1:4) .eq. 'TRIA') then
                        nsur=3
                        pt2(1)=connex(zi(jconn2+jel-1)+1-1)
                        pt2(2)=connex(zi(jconn2+jel-1)+2-1)
                        pt2(3)=connex(zi(jconn2+jel-1)+3-1)
                        pt2(4)=connex(zi(jconn2+jel-1)+1-1)
                    else if (typema(jel)(1:4).eq.'QUAD') then
                        nsur=4
                        pt2(1)=connex(zi(jconn2+jel-1)+1-1)
                        pt2(2)=connex(zi(jconn2+jel-1)+2-1)
                        pt2(3)=connex(zi(jconn2+jel-1)+3-1)
                        pt2(4)=connex(zi(jconn2+jel-1)+4-1)
                        pt2(5)=connex(zi(jconn2+jel-1)+1-1)
                    else
                        goto 50
                    endif
                else
                    if (typema(jel)(1:5) .eq. 'TETRA') then
                        nsur=4
                        pt2(1)=connex(zi(jconn2+jel-1)+1-1)
                        pt2(2)=connex(zi(jconn2+jel-1)+2-1)
                        pt2(3)=connex(zi(jconn2+jel-1)+3-1)
                        pt2(4)=connex(zi(jconn2+jel-1)+1-1)
                        pt2(5)=connex(zi(jconn2+jel-1)+3-1)
                        pt2(6)=connex(zi(jconn2+jel-1)+4-1)
                        pt2(7)=connex(zi(jconn2+jel-1)+2-1)
                        pt2(8)=connex(zi(jconn2+jel-1)+3-1)
                        pt2(9)=connex(zi(jconn2+jel-1)+4-1)
                        pt2(10)=connex(zi(jconn2+jel-1)+1-1)
                        pt2(11)=connex(zi(jconn2+jel-1)+2-1)
                        pt2(12)=connex(zi(jconn2+jel-1)+4-1)
                    else if (typema(jel)(1:4).eq.'HEXA') then
                        nsur=6
                        pt2(1)= connex(zi(jconn2+jel-1)+1-1)
                        pt2(2)= connex(zi(jconn2+jel-1)+2-1)
                        pt2(3)= connex(zi(jconn2+jel-1)+3-1)
                        pt2(4)= connex(zi(jconn2+jel-1)+4-1)
                        pt2(5)= connex(zi(jconn2+jel-1)+5-1)
                        pt2(6)= connex(zi(jconn2+jel-1)+6-1)
                        pt2(7)= connex(zi(jconn2+jel-1)+7-1)
                        pt2(8)= connex(zi(jconn2+jel-1)+8-1)
                        pt2(9)= connex(zi(jconn2+jel-1)+1-1)
                        pt2(10)=connex(zi(jconn2+jel-1)+2-1)
                        pt2(11)=connex(zi(jconn2+jel-1)+6-1)
                        pt2(12)=connex(zi(jconn2+jel-1)+5-1)
                        pt2(13)=connex(zi(jconn2+jel-1)+4-1)
                        pt2(14)=connex(zi(jconn2+jel-1)+3-1)
                        pt2(15)=connex(zi(jconn2+jel-1)+7-1)
                        pt2(16)=connex(zi(jconn2+jel-1)+8-1)
                        pt2(17)=connex(zi(jconn2+jel-1)+2-1)
                        pt2(18)=connex(zi(jconn2+jel-1)+3-1)
                        pt2(19)=connex(zi(jconn2+jel-1)+7-1)
                        pt2(20)=connex(zi(jconn2+jel-1)+6-1)
                        pt2(21)=connex(zi(jconn2+jel-1)+1-1)
                        pt2(22)=connex(zi(jconn2+jel-1)+4-1)
                        pt2(23)=connex(zi(jconn2+jel-1)+8-1)
                        pt2(24)=connex(zi(jconn2+jel-1)+5-1)
                    else
                        goto 50
                    endif
                endif
                do 60 isur = 1, nsur
                    if (ndim .eq. 2) then
                        test=(n1.eq.pt2(isur).or.n2.eq.pt2(isur))&
                        .and. (n1.eq.pt2(isur+1).or.n2.eq.pt2(isur+1))
                    else
                        if (typema(inel)(1:5) .eq. 'TETRA') then
                            test=( n1.eq.pt2(3*(isur-1)+1) .or.&
                            n2.eq.pt2(3*(isur-1)+1) .or. n3.eq.pt2(3*(&
                            isur-1)+1)) .and. ( n1.eq.pt2(3*(isur-1)+&
                            2) .or. n2.eq.pt2(3*(isur-1)+2) .or.&
                            n3.eq.pt2(3*(isur-1)+2)) .and. (&
                            n1.eq.pt2(3*(isur-1)+3) .or. n2.eq.pt2(3*(&
                            isur-1)+3) .or. n3.eq.pt2(3*(isur-1)+3))
                        endif
                        if (typema(inel)(1:4) .eq. 'HEXA') then
                            test=( n1.eq.pt2(4*(isur-1)+1) .or.&
                            n2.eq.pt2(4*(isur-1)+1) .or. n3.eq.pt2(4*(&
                            isur-1)+1) .or. n4.eq.pt2(4*(isur-1)+1))&
                            .and. ( n1.eq.pt2(4*(isur-1)+2) .or.&
                            n2.eq.pt2(4*(isur-1)+2) .or. n3.eq.pt2(4*(&
                            isur-1)+2) .or. n4.eq.pt2(4*(isur-1)+2))&
                            .and. ( n1.eq.pt2(4*(isur-1)+3) .or.&
                            n2.eq.pt2(4*(isur-1)+3) .or. n3.eq.pt2(4*(&
                            isur-1)+3 ) .or. n4.eq.pt2(4*(isur-1)+3))&
                            .and. ( n1.eq.pt2(4*(isur-1)+4) .or.&
                            n2.eq.pt2(4*(isur-1)+4) .or. n3.eq.pt2(4*(&
                            isur-1)+4) .or. n4.eq.pt2(4*(isur-1)+4))
                        endif
                    endif
                    if (test) goto 40
60              continue
50          continue
            nomili(n1)=2
            nomili(n2)=2
            if (ndim .eq. 3) then
                nomili(n3)=2
                if (typema(inel)(1:4) .eq. 'HEXA') nomili(n4)=2
            endif
40      continue
!
70      continue
!
! ORDRE
!
        if (ordre .eq. 0) then
            if (typema(inel)(1:5) .eq. 'TRIA3' .or. typema(inel)(1:5) .eq. 'QUAD4' .or.&
                typema(inel)(1:6) .eq. 'TETRA4' .or. typema(inel)(1:5) .eq. 'HEXA8') ordre=1
            if (typema(inel)(1:5) .eq. 'TRIA6' .or. typema(inel)(1:5) .eq. 'QUAD8' .or.&
                typema(inel)(1:5) .eq. 'QUAD9' .or. typema(inel)(1:7) .eq. 'TETRA10' .or.&
                typema(inel)(1:6) .eq. 'HEXA20' .or. typema(inel)(1:6) .eq. 'HEXA27') ordre=2
        endif
!
! NOEUD MILIEU
!
        if (typema(inel)(1:4) .eq. 'SEG3' .or.&
            typema(inel)(1:5) .eq. 'TRIA6' .or. typema(inel)(1:5) .eq. 'QUAD8' .or.&
            typema(inel)(1:5) .eq. 'QUAD9' .or. typema(inel)(1:7) .eq. 'TETRA10' .or.&
            typema(inel)(1:6) .eq. 'HEXA20' .or. typema(inel)(1:6) .eq. 'HEXA27') then
!          * preparation appel a nomil pour recuperer les noeuds milieux de chaque arete
           typma2=typema(inel)
           complet=.false.
!          * passage elements complets vers elements incomplets pour appel a nomil
           if (typema(inel)(1:5) .eq. 'QUAD9') then
                 typma2='QUAD8'
                 complet=.true.
           endif
           if (typema(inel)(1:6) .eq. 'HEXA27') then
                 typma2='HEXA20'
                 complet=.true.
           endif

           call nomil(typma2, nm, nbar)

!          * mise a 0 de nomili pour les noeuds milieux des aretes
           do ia=1, nbar
               nomili(connex(zi(jconn2+inel-1)+nm(ia)-1))=0
           enddo
!          * traitement des noeuds centraux pour les elements complets
           if (complet) then
              if (typema(inel)(1:5) .eq. 'QUAD9') then
                  nomili(connex(zi(jconn2+inel-1)+9-1))=0
              endif
              if (typema(inel)(1:6) .eq. 'HEXA27') then
                  nomili(connex(zi(jconn2+inel-1)+21-1))=0
                  nomili(connex(zi(jconn2+inel-1)+22-1))=0
                  nomili(connex(zi(jconn2+inel-1)+23-1))=0
                  nomili(connex(zi(jconn2+inel-1)+24-1))=0
                  nomili(connex(zi(jconn2+inel-1)+25-1))=0
                  nomili(connex(zi(jconn2+inel-1)+26-1))=0
                  nomili(connex(zi(jconn2+inel-1)+27-1))=0
              endif
           endif
        endif
!
20  end do
!
! 5 - CALCUL DE NELCOM ET NBEF(INNO)
! NELCOM     : NBRE MAX D EFS UTILES CONNECTES AUX NOEUDS SOMMETS
! NBRE       : NBRE D EF CONNECTES AU NOEUD INNO
! NBEF(INNO) : NBRE D EFS UTILES CONNECTES AU NOEUD INNO SOMMET
!
    nelcom=0
    do 80 inno = 1, nnoem
        nbef(inno)=0
        if (nomili(inno) .ne. 0) then
            nbre=zi(jcinv2+inno)-zi(jcinv2+inno-1)
            do 90 inel = 1, nbre
                nuef = zi(jcinv1-1+zi(jcinv2+inno-1)+inel-1)
                if (ndim .eq. 2) then
                    if (typema(nuef)(1:4) .eq. 'TRIA' .or. typema(nuef)( 1:4) .eq. 'QUAD') then
                        nbef(inno)=nbef(inno)+1
                    endif
                else
                    if (typema(nuef)(1:5) .eq. 'TETRA' .or. typema(nuef)( 1:4) .eq. 'HEXA') then
                        nbef(inno)=nbef(inno)+1
                    endif
                endif
90          continue
            nelcom=max(nelcom,nbef(inno))
        endif
80  end do
!
! 5 - OBJET '&&SINGUM.DIME'
!
    chdime='&&SINGUM.DIME           '
    call wkvect(chdime, 'V V I', 3, jdime)
    zi(jdime+1-1)=nsommx
    zi(jdime+2-1)=nelcom
    zi(jdime+3-1)=ordre
!
! 6 - OBJET '&&SINGUM.CINV'
!     1ERE VALEUR = NBRE D EFS UTILES CONNECTES AU NOEUD N°X
!     2EME VALEUR = 0 NOEUD MILIEU OU NON CONNECTE A UN EF UTILE
!                   1 NOEUD SOMMET A L INTERIEUR + LIE A UN EF UTILE
!                   2 NOEUD SOMMET BORD + LIE A UN EF UTILE
!     CONNECTIVITE  NOEUD N°X=>N° DES EF CONNECTES A X
!
    chcinv='&&SINGUM.CINV           '
    call wkvect(chcinv, 'V V I', nnoem*(nelcom+2), jcinv)
!
    do 100 inno = 1, nnoem
        nbre=zi(jcinv2+inno)-zi(jcinv2+inno-1)
        adress=jcinv+(nelcom+2)*(inno-1)
        zi(adress+1-1)=nbef(inno)
        zi(adress+2-1)=0
        i=1
        do 110 inel = 1, nbre
            nuef = zi(jcinv1-1+zi(jcinv2+inno-1)+inel-1)
            if (ndim .eq. 2) then
                if (typema(nuef)(1:4) .eq. 'TRIA' .or. typema(nuef)(1:4) .eq. 'QUAD') then
                    if (nomili(inno) .ne. 0) zi(adress+2-1)=nomili(inno)
                    zi(adress+i+2-1)=nuef
                    i=i+1
                endif
            else
                if (typema(nuef)(1:5) .eq. 'TETRA' .or. typema(nuef)(1:4) .eq. 'HEXA') then
                    if (nomili(inno) .ne. 0) zi(adress+2-1)=nomili(inno)
                    zi(adress+i+2-1)=nuef
                    i=i+1
                endif
            endif
110      continue
100  end do
!
    do 120 inno = 1, nnoem
        adress=jcinv+(nelcom+2)*(inno-1)
120  end do
!
    call jedetr(cinv)
    call jedema()
!
end subroutine
