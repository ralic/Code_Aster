subroutine projkm(nmabet, nbmabe, nbnobe, mailla, caelem, dmax_cable, &
                  nnoeca, x3dca, noebe, numail, nbcnx,&
                  cxma, xyzma, normal, itria, xbar,&
                  iproj, excent)
    implicit none
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!  DESCRIPTION : TENTATIVE DE PROJECTION D'UN NOEUD CABLE SUR LES
!  -----------   MAILLES APPARTENANT A LA STRUCTURE BETON
!                APPELANT : PROJCA
!
!  IN     : NMABET : CHARACTER*24 ,
!                    OBJET CONTENANT LES MAILLES BETON
!  IN     : NBMABE : INTEGER , SCALAIRE
!                    NOMBRE DE MAILLE BETON
!  IN     : NBNOBE : INTEGER , SCALAIRE
!                    NOMBRE DE NOEUD BETON
!  IN     : MAILLA : CHARACTER*8 , SCALAIRE
!                    NOM DU CONCEPT MAILLAGE ASSOCIE A L'ETUDE
!  IN     : CAELEM : CHARACTER*8 , SCALAIRE
!                    NOM DU CONCEPT CARA_ELEM ASSOCIE A L'ETUDE
!  IN     : X3DCA  : REAL*8 , VECTEUR DE DIMENSION 3
!                    COORDONNEES DU NOEUD CABLE CONSIDERE
!  IN     : NOEBE  : INTEGER , SCALAIRE
!                    NUMERO DU NOEUD BETON LE PLUS PROCHE DU NOEUD CABLE
!                    CONSIDERE
!  OUT    : NUMAIL : INTEGER , SCALAIRE
!                    SI PROJECTION REUSSIE : NUMERO DE LA MAILLE SUR
!                    LAQUELLE EST REALISEE LA PROJECTION
!  OUT    : NBCNX  : INTEGER , SCALAIRE
!                    SI PROJECTION REUSSIE : NOMBRE DE NOEUDS DE LA
!                    MAILLE SUR LAQUELLE EST REALISEE LA PROJECTION
!  OUT    : CXMA   : INTEGER , VECTEUR DE DIMENSION AU PLUS NNOMAX
!                    SI PROJECTION REUSSIE : NUMEROS DES NOEUDS DE LA
!                    MAILLE SUR LAQUELLE EST REALISEE LA PROJECTION
!                    (TABLE DE CONNECTIVITE)
!  OUT    : XYZMA  : REAL*8 , TABLEAU DE DIMENSIONS (3,NNOMAX)
!                    SI PROJECTION REUSSIE : TABLEAU DES COORDONNEES
!                    DES NOEUDS DE LA MAILLE SUR LAQUELLE EST REALISEE
!                    LA PROJECTION
!  OUT    : NORMAL : REAL*8 , VECTEUR DE DIMENSION 3
!                    SI PROJECTION REUSSIE : COORDONNEES DANS LE REPERE
!                    GLOBAL DU VECTEUR NORMAL AU PLAN MOYEN DE LA MAILLE
!                    SUR LAQUELLE EST REALISEE LA PROJECTION
!  OUT    : ITRIA  : INTEGER , SCALAIRE
!                    SI PROJECTION REUSSIE : INDICATEUR DU SOUS-DOMAINE
!                    AUQUEL APPARTIENT LE POINT PROJETE :
!                    ITRIA = 1 : TRIANGLE 1-2-3
!                    ITRIA = 2 : TRIANGLE 3-4-1
!  OUT    : XBAR   : REAL*8 , VECTEUR DE DIMENSION 3
!                    SI PROJECTION REUSSIE : COORDONNEES BARYCENTRIQUES
!                    DU POINT PROJETE (BARYCENTRE DES SOMMETS DU
!                    TRIANGLE 1-2-3 OU 3-4-1)
!  OUT    : IPROJ  : INTEGER , SCALAIRE
!                    INDICE DE PROJECTION
!                    IPROJ = -1  PROJECTION NON REUSSIE
!                    IPROJ =  0  LE POINT PROJETE EST A L'INTERIEUR
!                                DE LA MAILLE
!                    IPROJ =  1X LE POINT PROJETE EST SUR UNE FRONTIERE
!                                DE LA MAILLE
!                    IPROJ =  2  LE POINT PROJETE COINCIDE AVEC UN DES
!                                NOEUDS DE LA MAILLE
!  OUT    : EXCENT : REAL*8 , SCALAIRE
!                    SI PROJECTION REUSSIE : EXCENTRICITE DU NOEUD
!                    CABLE PAR RAPPORT A LA MAILLE SUR LAQUELLE EST
!                    REALISEE LA PROJECTION
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
!
! ARGUMENTS
! ---------
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/canorm.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/normev.h"
#include "asterfort/projsg.h"
#include "asterfort/projtq.h"
#include "asterfort/recu_cara_ma.h"
#include "asterfort/utmess.h"
#include "asterfort/veri_seg.h"
#include "asterfort/veri_noe.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/dscal.h"
    character(len=8) :: mailla, caelem, nnoeca
    integer :: noebe, numail, nbcnx, cxma(*), itria, iproj, nbmabe, nbnobe
    real(kind=8) :: x3dca(*), xyzma(3, *), normal(*), xbar(*), excent
    real(kind=8) :: dmax_cable
    character(len=24) :: nmabet
!
! VARIABLES LOCALES
! -----------------
    integer :: icnx, imail, inoma, jcoor, jlnuma, jnumab, jtyma
    integer :: nbmaok, noe, ntyma, jconx1, jconx2, nblinobet2, jlinob2
    integer :: nblinoold, nblinobet1, jlinob1, inob, imabok, jno, kno
    integer :: nbmaok1, jliproj, jlinoma, inoeu, icote, inoeu2, n1, n2, iproj2
    real(kind=8) :: d, dmax, dx, dy, dz, epsg, x3dp(3), ep_ma, exc_ma, xbar2(2)
    real(kind=8) :: xbetp(3), exc_max
    character(len=8) :: nomma
    character(len=19) :: carte
    character(len=24) :: conxma, coorno, tymama, linobet2, linobet1, nomama
    character(len=24) :: lnuma, liproj, linoma
    aster_logical :: lrechelarg, lnopres
!
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 1   ACCES AUX OBJETS DU CONCEPT MAILLAGE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    conxma = mailla//'.CONNEX'
    call jeveuo(conxma, 'L', jconx1)
    coorno = mailla//'.COORDO    .VALE'
    call jeveuo(coorno, 'L', jcoor)
    call jeveuo(nmabet, 'L', jnumab)
    tymama = mailla//'.TYPMAIL'
    call jeveuo(tymama, 'L', jtyma)
    nomama = mailla//'.NOMMAI'
    carte=caelem//'.CARCOQUE  '
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 2   TENTATIVE DE PROJECTION DU NOEUD CABLE CONSIDERE SUR LES MAILLES
!     APPARTENANT A LA STRUCTURE BETON
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    epsg = 1.0d+08 * r8prem()
    nbmaok = 0
    nbmaok1 = 0
!    
    lnuma = '&&PROJKM.NUMA_NOEBE'
    call jecreo(lnuma, 'V V I')
    call jeecra(lnuma, 'LONMAX', nbmabe)
! 
    liproj = '&&PROJKM.IPROJ_NOEBE'
    call jecreo(liproj, 'V V I')
    call jeecra(liproj, 'LONMAX', nbmabe)
! 
    linoma = '&&PROJKM.DOUBNO_NOEBE'
    call jecreo(linoma, 'V V I')
    call jeecra(linoma, 'LONMAX', 3*nbmabe)
    
!   calcul de l'excentrement max
    xbetp(1) = zr(jcoor+3*(noebe-1) )
    xbetp(2) = zr(jcoor+3*(noebe-1)+1)
    xbetp(3) = zr(jcoor+3*(noebe-1)+2)
    exc_max = sqrt((xbetp(1)-x3dca(1))**2 + (xbetp(2)-x3dca(2))**2&
                +(xbetp(3)-x3dca(3))**2)
!
!     1er passage :
!.... BOUCLE SUR LES MAILLES APPARTENANT A LA STRUCTURE BETON, POUR
!.... RETROUVER LE NOEUD BETON LE PLUS PROCHE DANS LES CONNECTIVITES
!
!     2eme passage si besoin:
!.... BOUCLE SUR LES MAILLES APPARTENANT A LA STRUCTURE BETON, POUR
!.... RETROUVER LES NOEUDS BETON DE LA LISTE LINOBET DANS LES CONNECTIVITES
!     le liste linobet2 contient les noeuds de toutes les mailles contenant
!     également le noeud le plus proche (deuxième cercle de recherche)
    lrechelarg = .false.
!
    linobet1 = '&&PROJKM.LINOBET1'
    nblinobet1 = 0
    call jecreo(linobet1, 'V V I')
    call jeecra(linobet1, 'LONMAX', nbnobe)
    nblinobet1 = 1
    call jeecra(linobet1, 'LONUTI', nblinobet1)
    call jeveuo(linobet1, 'E', jlinob1)
    zi(jlinob1) = noebe
!
    linobet2 = '&&PROJKM.LINOBET2'
    nblinobet2 = 0
    nblinoold = 0
    call jecreo(linobet2, 'V V I')
    call jeecra(linobet2, 'LONMAX', nbnobe)
!
    call jeveuo(jexatr(mailla//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
88  continue   
!
    do 10 imail = 1, nbmabe
!
        numail = zi(jnumab+imail-1)
        if (lrechelarg)then
            call jeveuo(lnuma, 'E', jlnuma)
            do imabok = 1, nbmaok1
                if (numail .eq. zi(jlnuma-1+imabok))then
                    call jelibe(lnuma)
                    goto 10
                endif
            enddo
        endif
        nbcnx = zi(jconx2+numail)-zi(jconx2-1+numail)
!
        do 20 icnx = 1, nbcnx
!
!.......... SI LE NOEUD BETON EST RETROUVE DANS LES CONNECTIVITES,
!.......... TEST DE PROJECTION DU NOEUD CABLE SUR LA MAILLE COURANTE
!
          do inob = 1, nblinobet1
            
            if (zi(jconx1-1+zi(jconx2+numail-1)+icnx-1) .eq. zi(jlinob1-1+inob)) then
!
!............. ON NOTE LE NUMERO DE LA MAILLE ET L'INDICE DU NOEUD
!............. NOEBE DANS LA TABLE DE CONNECTIVITE ASSOCIEE
!
                nbmaok = nbmaok + 1
                call jeecra(lnuma, 'LONUTI', nbmaok)
                call jeveuo(lnuma, 'E', jlnuma)
                zi(jlnuma+nbmaok-1) = numail
!
                call jeecra(liproj, 'LONUTI', nbmaok)
                call jeveuo(liproj, 'E', jliproj)
!               initialisation à -3, rien ne doit rester à -3
                zi(jliproj+nbmaok-1) = -3
!
                call jeecra(linoma, 'LONUTI', 3*nbmaok)
                call jeveuo(linoma, 'E', jlinoma)
                zi(jlinoma+3*nbmaok-3) = 0
                zi(jlinoma+3*nbmaok-2) = 0
                zi(jlinoma+3*nbmaok-1)   = 0
!
                if (.not. lrechelarg) then
                    nblinoold = nblinobet2
                    call jeecra(linobet2, 'LONUTI', nblinobet2 + nbcnx - 1)
                    call jeveuo(linobet2, 'E', jlinob2)
                endif
!
!............. RECUPERATION DES NUMEROS ET DES COORDONNEES DES NOEUDS
!............. DE LA MAILLE
!
                do inoma = 1, nbcnx
                    noe = zi(jconx1-1+zi(jconx2+numail-1)+inoma-1)
                    cxma(inoma) = noe
                    xyzma(1,inoma) = zr(jcoor+3*(noe-1) )
                    xyzma(2,inoma) = zr(jcoor+3*(noe-1)+1)
                    xyzma(3,inoma) = zr(jcoor+3*(noe-1)+2)
                    if ((.not. lrechelarg) .and. inoma.ne.icnx)then
                        lnopres = .false.
                        do jno = 1, nblinoold
                            if(noe.eq.zi(jlinob2-1+jno))then
                                lnopres = .true.
                                exit
                            endif
                        enddo
                        if (.not. lnopres) then
                            zi(jlinob2+nblinobet2) = noe
                            nblinobet2 = nblinobet2 + 1
                        endif
                    endif
                enddo
                if (.not. lrechelarg) then
                    call jelibe(linobet2)
                endif
!
!............. RECUPERATION DE LA NORMALE AU PLAN DE LA MAILLE
!
                ntyma = zi(jtyma+numail-1)
                call canorm(xyzma(1, 1), normal(1), 3, ntyma, 1)
!
!............. EXCENTRICITE DU NOEUD DU CABLE ET COORDONNEES
!............. DU POINT PROJETE
!
                excent = normal(1)*(x3dca(1)-xyzma(1,1)) + normal(2)*( x3dca(2)-xyzma(2,1)) + nor&
                         &mal(3)*(x3dca(3)-xyzma(3,1))
!               l'excentrement ne peut pas etre plus grand que la distance 
!               au noeud de beton le plus proche 
                if (abs(excent).gt.exc_max)then
                    zi(jliproj+nbmaok-1) = -1
                    call jelibe(lnuma)
                    call jelibe(liproj)
                    call jelibe(linoma)
                    goto 10
                endif
!               verif de la compatibilité avec épaisseur et excentrement de la maille
!               
                call recu_cara_ma(mailla, carte, numail, 'EP      ', ep_ma)
                call recu_cara_ma(mailla, carte, numail, 'EXCENT  ', exc_ma)
!
                if ((excent .gt. exc_ma +ep_ma/2) .or. (excent.lt. exc_ma - ep_ma/2)) then
                    zi(jliproj+nbmaok-1) = -2
                    call jelibe(lnuma)
                    call jelibe(liproj)
                    call jelibe(linoma)
                    goto 10
                endif
!
                dmax = 0.0d0
                do inoma = 1, nbcnx
                    dx = x3dca(1) - xyzma(1,inoma)
                    dy = x3dca(2) - xyzma(2,inoma)
                    dz = x3dca(3) - xyzma(3,inoma)
                    d = dble ( sqrt ( dx*dx + dy*dy + dz*dz ) )
                    if (d .gt. dmax) dmax = d
                enddo
                if (dmax .eq. 0.0d0) dmax = 1.0d0
                if (dble(abs(excent))/dmax .lt. epsg) excent = 0.0d0
                call dcopy(3, x3dca(1), 1, x3dp(1), 1)  
                if (excent .ne. 0.0d0) then
                    call daxpy(3, -excent, normal(1), 1, x3dp(1),&
                               1)
                    if (excent .lt. 0.0d0) then
                        excent = dble(abs(excent))
                        call dscal(3, -1.0d0, normal(1), 1)
                    endif
                endif
!
!............. TEST D'APPARTENANCE DU POINT PROJETE AU DOMAINE
!............. GEOMETRIQUE DEFINI PAR LA MAILLE
!
                call projtq(nbcnx, xyzma(1, 1), icnx, x3dp(1), excent, &
                            itria, inoeu, icote, xbar(1), iproj)
                if (iproj .ge. 0 .and. iproj .lt. 20) then
                    goto 999 
                elseif (iproj .ge. 20)then
                    if (lrechelarg) then
                        nbmaok = nbmaok + 1
                        call jeecra(lnuma, 'LONUTI', nbmaok)
                        call jeveuo(lnuma, 'E', jlnuma)                
!
                        call jeecra(liproj, 'LONUTI', nbmaok)
                        call jeveuo(liproj, 'E', jliproj)    
!
                        call jeecra(linoma, 'LONUTI', 3*nbmaok)
                        call jeveuo(linoma, 'E', jlinoma)
                    endif
!
                    zi(jlnuma+nbmaok-1) = numail
                    zi(jliproj+nbmaok-1) = iproj
                    if (iproj.eq.20)then    
                        zi(jlinoma+3*nbmaok-3) = cxma(icote)
                        icote = icote +1
                        if (icote.gt.nbcnx) icote = 1
                        zi(jlinoma+3*nbmaok-2)= cxma(icote)
                        zi(jlinoma+3*nbmaok-1)  = 0
                    else
!                       on stocke le noeud et ses deux voisins
                        zi(jlinoma+3*nbmaok-3) = cxma(inoeu)
                        inoeu2 = inoeu+1
                        if (inoeu2.gt.nbcnx) inoeu2 = 1
                        zi(jlinoma+3*nbmaok-2) = cxma(inoeu2)
                        inoeu2 =inoeu-1
                        if (inoeu2.eq.0) inoeu2 = nbcnx
                        zi(jlinoma+3*nbmaok-1)   = cxma(inoeu2)
                    endif
!
                    call jelibe(lnuma)
                    call jelibe(liproj)
                    call jelibe(linoma)
                    goto 10
                else
                    zi(jliproj+nbmaok-1) = -1
                    call jelibe(lnuma)
                    call jelibe(liproj)
                    call jelibe(linoma)
                    goto 10
                endif
!
            endif
          enddo
20      continue
10  continue
!   recherche elargie si pas déjà fait
!    if (.false.) then
    if (.not. lrechelarg) then
        lrechelarg = .true.
        nbmaok1 = nbmaok
!       copie de linobet2 vers linobet1
        call jelibe(linobet1)
        call jeecra(linobet1, 'LONUTI', nblinobet2)
        call jeveuo(linobet1, 'E', jlinob1)
        call jeveuo(linobet2, 'E', jlinob2)
        do inob = 1, nblinobet2
            zi(jlinob1-1+inob) = zi(jlinob2-1+inob)
        enddo
        nblinobet1 = nblinobet2
        goto 88
    endif
!
!   analyse des projections possibles sur segments et noeuds
!   
    call jeveuo(lnuma, 'E', jlnuma)
    call jeveuo(liproj, 'E', jliproj)
    call jeveuo(linoma, 'E', jlinoma)
!
!   segments
!
    call veri_seg(mailla, dmax_cable, zi(jlnuma), zi(jliproj), zi(jlinoma),&
                   nbmaok, x3dca, iproj, n1, n2, numail)
    if (iproj .eq. 0)then
!       on se recolle au cas iproj = 1* pour reci2d
        nbcnx = zi(jconx2+numail)-zi(jconx2-1+numail)
        jno = 0
        kno = 0
        do inoma = 1, nbcnx
            noe = zi(jconx1-1+zi(jconx2+numail-1)+inoma-1)
            cxma(inoma) = noe
            xyzma(1,inoma) = zr(jcoor+3*(noe-1) )
            xyzma(2,inoma) = zr(jcoor+3*(noe-1)+1)
            xyzma(3,inoma) = zr(jcoor+3*(noe-1)+2)
            if (n1.eq.noe) then
                jno = inoma
            endif
        enddo
        kno = jno + 1
        if (kno .gt.nbcnx) kno = 1
        if (n2 .ne. cxma(kno))then
            kno = jno
            jno = kno -1
            if (jno .eq. 0) jno = nbcnx
            ASSERT(cxma(jno).eq. n2)
        endif
        iproj = 10 + jno
        if (nbcnx .eq. 3) then 
            itria = 1
        else
            if (jno .eq. 1 .or. jno .eq. 2) itria = 1
            if (jno .eq. 3 .or. jno .eq. 4) itria = 2
        endif
!
        call projsg(x3dca, xyzma(1, jno), xyzma(1, kno), normal, x3dp,&
                        xbar2, iproj2, excent)
        if (jno.eq.1 .or. (jno.eq.3 .and. nbcnx .eq. 4)) then
            xbar(1) = xbar2(1)
            xbar(2) = xbar2(2)
            xbar(3) = 0.d0
        elseif (jno.eq.2 .or. (jno.eq.4 .and. nbcnx .eq. 4)) then
            xbar(1) = 0.d0
            xbar(2) = xbar2(1)
            xbar(3) = xbar2(2)
        elseif (jno.eq.3) then
            xbar(1) = xbar2(2)
            xbar(2) = 0.d0
            xbar(3) = xbar2(1)
        endif
    endif
!
!   noeuds
    if (iproj.eq.-1)then
!       on se recolle au cas iproj = 2 pour reci2d
        call veri_noe(mailla, dmax_cable, zi(jlnuma), zi(jliproj),&
                      nbmaok, x3dca, iproj, noe, numail)
        if (iproj .eq. 0)then
            ASSERT(noe.eq.noebe)
!
            xyzma(1,1) = zr(jcoor+3*(noe-1) )
            xyzma(2,1) = zr(jcoor+3*(noe-1)+1)
            xyzma(3,1) = zr(jcoor+3*(noe-1)+2)
!
            normal(1) = x3dca(1) - xyzma(1,1)
            normal(2) = x3dca(2) - xyzma(2,1)
            normal(3) = x3dca(3) - xyzma(3,1)
!
            call normev(normal, excent)
            if (abs(excent) .lt. epsg) excent = 0.0d0
!
            iproj = 2
        endif
    endif
!
!   erreur sur l'excentrement
    if (iproj.eq.-1)then
        do imail=1, nbmaok1
            if (zi(jliproj-1+imail).eq.-3)then
                ASSERT(.false.)
            elseif (zi(jliproj-1+imail).eq.-2)then
                numail = zi(jlnuma-1+imail)
                nbcnx = zi(jconx2+numail)-zi(jconx2-1+numail)
                do inoma = 1, nbcnx
                    noe = zi(jconx1-1+zi(jconx2+numail-1)+inoma-1)
                    xyzma(1,inoma) = zr(jcoor+3*(noe-1) )
                    xyzma(2,inoma) = zr(jcoor+3*(noe-1)+1)
                    xyzma(3,inoma) = zr(jcoor+3*(noe-1)+2)
                enddo
                ntyma = zi(jtyma+numail-1)
                call canorm(xyzma, normal, 3, ntyma, 1)
!
                excent = normal(1)*(x3dca(1)-xyzma(1,1)) + normal(2)*( x3dca(2)-xyzma(2,1)) &
                             + normal(3)*(x3dca(3)-xyzma(3,1))
                call dcopy(3, x3dca, 1, x3dp, 1)
                call daxpy(3, -excent, normal, 1, x3dp, 1)
                call projtq(nbcnx, xyzma(1, 1), icnx, x3dp, abs(excent), &
                                itria, inoeu, icote, xbar, iproj)
                if (iproj.ge.0)then
                    call recu_cara_ma(mailla, carte, numail, 'EP      ', ep_ma)
                    call recu_cara_ma(mailla, carte, numail, 'EXCENT  ', exc_ma)
                    call jenuno(jexnum(nomama, numail), nomma)
                    call utmess('F','MODELISA5_51', nk = 2, valk = [nnoeca,nomma],&
                                nr = 3, valr = [excent, exc_ma +ep_ma/2, exc_ma - ep_ma/2])
                endif
            endif
        enddo
    endif
!
999 continue
    call jedetr(linobet2)
    call jedetr(linobet1)
    call jedetr(lnuma)
    call jedetr(liproj)
    call jedetr(linoma)
    
    call jedema()
!
! --- FIN DE PROJKM.
end subroutine
