subroutine drz13d(lisnoz, lonlis, chargz, typlaz, lisrez)
    implicit none
!
#include "asterc/getres.h"
#include "asterc/indik8.h"
#include "asterfort/afrela.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: lonlis
    character(len=*) :: lisnoz, chargz, typlaz, lisrez
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! --- ------------------------------------------------------------------
!
!     BLOCAGE DES DEPLACEMENTS RELATIFS D'UNE LISTE DE NOEUDS
!     SPECIFIEE PAR L'UTILISATEUR DANS LE CAS OU L' ON EST
!     EN 3D ET IL EXISTE AU-MOINS UN NOEUD PORTANT LES 3 DDLS
!     DE ROTATION DRX, DRY ET DRZ
!
! --- ------------------------------------------------------------------
!  IN
!     LISNOZ [K24]   : NOM DE LA LISTE DES NOEUDS A LIER
!     LONLIS [I  ]   : LONGUEUR DE LA LISTE DES NOEUDS A LIER
!     CHARGZ [K8 ]   : NOM DE LA SD CHARGE
!     TYPLAZ [K2]    : TYPE DES MULTIPLICATEURS DE LAGRANGE
!                      ASSOCIES A LA RELATION :
!                      SI = '12'  LE PREMIER LAGRANGE EST AVANT
!                                 LE NOEUD PHYSIQUE
!                                 LE SECOND LAGRANGE EST APRES
!                      SI = '22'  LE PREMIER LAGRANGE EST APRES
!                                 LE NOEUD PHYSIQUE
!                                 LE SECOND LAGRANGE EST APRES
!
!     LISREZ [K19]   : NOM DE LA SD LISTE DE RELATIONS
!
! --- ------------------------------------------------------------------
#include "jeveux.h"
!
    integer :: nmocl
    parameter     (nmocl=300)
!
    integer :: i, ibid, icmp1, icmp2, icmp3, icmp4, icmp5
    integer :: icmp6, ier, ierd, ilisno, in, ino1, inom
    integer :: j, jcoor, jliscc, jliscr, jlisdi, jlisdl, jlisdm
    integer :: jlisno, jnoma, jprnm, nbcmp, nbec, nbterm
    integer :: nddla, vali(2)
    integer :: ntypel(nmocl)
!
!
    real(kind=8) :: beta, un, x, y, z
    complex(kind=8) :: betac
!
    character(len=1) :: k1bid
    character(len=2) :: typlag
    character(len=4) :: typval, typcoe
    character(len=8) :: betaf, resu, mod, nomg, nomnoe, k8bid
    character(len=8) :: noma, nomcmp(nmocl), cmp1, cmp2, cmp3, cmp4, cmp5, cmp6
    character(len=8) :: charge
    character(len=9) :: nomte
    character(len=16) :: type, oper
    character(len=19) :: ligrmo
    character(len=19) :: lisrel
    character(len=24) :: lisnoe
    character(len=32) :: kexnom
! --- ------------------------------------------------------------------
    call jemarq()
!
    call getres(resu, type, oper)
    lisrel = lisrez
    charge = chargz
    typlag = typlaz
    lisnoe = lisnoz
! --- INITIALISATIONS
    betaf = '&FOZERO'
    beta = 0.0d0
    betac = (0.0d0,0.0d0)
    un = 1.0d0
! --- MODELE ASSOCIE AU LIGREL DE CHARGE
    call dismoi('F', 'NOM_MODELE', charge(1:8), 'CHARGE', ibid,&
                mod, ier)
! ---  LIGREL DU MODELE
    ligrmo = mod(1:8)//'.MODELE'
! --- MAILLAGE ASSOCIE AU MODELE
    call jeveuo(ligrmo//'.LGRF', 'L', jnoma)
    noma = zk8(jnoma)
! --- TYPE DES VALEURS DES COEFFICIENTS DES RELATIONS
    typcoe = 'REEL'
! --- TYPE DES VALEURS AU SECOND MEMBRE DES RELATIONS
    if (oper(15:16) .eq. '_F') then
        typval = 'FONC'
    else if (oper(15:16).eq.'_C') then
        typval = 'COMP'
    else if (oper(15:16).eq.'  ') then
        typval = 'REEL'
    else
        ASSERT(.false.)
    endif
! --- RECUPERATION DES NOMS DES DDLS ET DES NUMEROS
! --- D'ELEMENTS DE LAGRANGE ASSOCIES
    nomg = 'DEPL_R'
    nomte = 'D_DEPL_R_'
!
    kexnom = jexnom('&CATA.GD.NOMCMP',nomg)
    call jeveuo(kexnom, 'L', inom)
    call jelira(kexnom, 'LONMAX', nbcmp, k1bid)
    nddla = nbcmp - 1
    if (nddla .gt. nmocl) then
        vali (1) = nmocl
        vali (2) = nddla
        call u2mesg('F', 'MODELISA8_29', 0, ' ', 2,&
                    vali, 0, 0.d0)
    endif
    do 10 i = 1, nddla
        nomcmp(i) = zk8(inom-1+i)
        kexnom = jexnom('&CATA.TE.NOMTE',nomte//nomcmp(i)(1:7))
        call jenonu(kexnom, ntypel(i))
10  end do
    call dismoi('F', 'NB_EC', nomg, 'GRANDEUR', nbec,&
                k8bid, ierd)
! --- ACCES A L'OBJET .PRNM
    if (nbec .gt. 10) then
        call u2mess('F', 'MODELISA_94')
    else
        call jeveuo(ligrmo//'.PRNM', 'L', jprnm)
    endif
! --- TABLEAUX DE TRAVAIL NECESSAIRES A L'AFFECTATION DE LISREL
! --- MAJORANT DU NOMBRE DE TERMES DANS UNE RELATION
    nbterm = 12
! --- VECTEUR DU NOM DES NOEUDS
    call wkvect('&&DRZ13D.LISNO', 'V V K8', nbterm, jlisno)
! --- VECTEUR DU NOM DES DDLS
    call wkvect('&&DRZ13D.LISDDL', 'V V K8', nbterm, jlisdl)
! --- VECTEUR DES COEFFICIENTS REELS
    call wkvect('&&DRZ13D.COER', 'V V R', nbterm, jliscr)
! --- VECTEUR DES COEFFICIENTS COMPLEXES
    call wkvect('&&DRZ13D.COEC', 'V V C', nbterm, jliscc)
! --- VECTEUR DES DIRECTIONS DES DDLS A CONTRAINDRE
    call wkvect('&&DRZ13D.DIRECT', 'V V R', 3*nbterm, jlisdi)
! --- VECTEUR DES DIMENSIONS DE CES DIRECTIONS
    call wkvect('&&DRZ13D.DIME', 'V V I', nbterm, jlisdm)
! --- RECUPERATION DU TABLEAU DES COORDONNEES
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
! --- LISTE DES NOEUDS A LIER, LA LISTE EST NON REDONDANTE
    call jeveuo(lisnoe, 'L', ilisno)
! --- ON REGARDE S'IL Y A UN NOEUD DE LA LISTE PORTANT LES 3 DDLS
! --- DE ROTATION
    cmp1 = 'DX'
    cmp2 = 'DY'
    cmp3 = 'DZ'
    cmp4 = 'DRX'
    cmp5 = 'DRY'
    cmp6 = 'DRZ'
!
    icmp1 = indik8(nomcmp,cmp1,1,nddla)
    icmp2 = indik8(nomcmp,cmp2,1,nddla)
    icmp3 = indik8(nomcmp,cmp3,1,nddla)
    icmp4 = indik8(nomcmp,cmp4,1,nddla)
    icmp5 = indik8(nomcmp,cmp5,1,nddla)
    icmp6 = indik8(nomcmp,cmp6,1,nddla)
    do 20 i = 1, lonlis
! ---    NUMERO DU NOEUD COURANT DE LA LISTE
        call jenonu(jexnom(noma//'.NOMNOE', zk8(ilisno+i-1)), in)
        if ((exisdg(zi(jprnm-1+(in-1)*nbec+1),icmp4)) .and.&
            (exisdg( zi(jprnm-1+(in-1)*nbec+1),icmp5)) .and.&
            (exisdg(zi(jprnm-1+( in-1)*nbec+1),icmp6))) then
            nomnoe = zk8(ilisno+i-1)
            ino1 = in
            goto 30
        endif
20  end do
    call u2mess('F', 'MODELISA4_44')
30  continue
!
    do 40 j = 1, lonlis
        if (zk8(ilisno+j-1) .eq. nomnoe) goto 40
        call jenonu(jexnom(noma//'.NOMNOE', zk8(ilisno+j-1)), in)
        x = zr(jcoor-1+3* (in-1)+1) - zr(jcoor-1+3* (ino1-1)+1)
        y = zr(jcoor-1+3* (in-1)+2) - zr(jcoor-1+3* (ino1-1)+2)
        z = zr(jcoor-1+3* (in-1)+3) - zr(jcoor-1+3* (ino1-1)+3)
        if ((exisdg(zi(jprnm-1+(in-1)*nbec+1),icmp1)) .and.&
            (exisdg( zi(jprnm-1+(in-1)*nbec+1),icmp2)) .and.&
            (exisdg(zi(jprnm-1+( in-1)*nbec+1),icmp3))) then
! ---       PREMIERE RELATION : DX(M) - DX(A) - Z*DRY(A) + Y*DRZ(A) =0
            nbterm = 4
            zk8(jlisno+1-1) = zk8(ilisno+j-1)
            zk8(jlisno+2-1) = nomnoe
            zk8(jlisno+3-1) = nomnoe
            zk8(jlisno+4-1) = nomnoe
            zk8(jlisdl+1-1) = 'DX'
            zk8(jlisdl+2-1) = 'DX'
            zk8(jlisdl+3-1) = 'DRY'
            zk8(jlisdl+4-1) = 'DRZ'
            zr(jliscr+1-1) = un
            zr(jliscr+2-1) = -un
            zr(jliscr+3-1) = -z
            zr(jliscr+4-1) = y
            call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                        zr(jlisdi), nbterm, beta, betac, betaf,&
                        typcoe, typval, typlag, 0.d0, lisrel)
! ---       DEUXIEME RELATION : DY(M) - DY(A) - X*DRZ(A) + Z*DRX(A) =0
            zk8(jlisdl+1-1) = 'DY'
            zk8(jlisdl+2-1) = 'DY'
            zk8(jlisdl+3-1) = 'DRZ'
            zk8(jlisdl+4-1) = 'DRX'
            zr(jliscr+1-1) = un
            zr(jliscr+2-1) = -un
            zr(jliscr+3-1) = -x
            zr(jliscr+4-1) = z
            call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                        zr(jlisdi), nbterm, beta, betac, betaf,&
                        typcoe, typval, typlag, 0.d0, lisrel)
! ---       TROISIEME RELATION : DZ(M) - DZ(A) - Y*DRX(A) + X*DRY(A) =0
            zk8(jlisdl+1-1) = 'DZ'
            zk8(jlisdl+2-1) = 'DZ'
            zk8(jlisdl+3-1) = 'DRX'
            zk8(jlisdl+4-1) = 'DRY'
            zr(jliscr+1-1) = un
            zr(jliscr+2-1) = -un
            zr(jliscr+3-1) = -y
            zr(jliscr+4-1) = x
            call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                        zr(jlisdi), nbterm, beta, betac, betaf,&
                        typcoe, typval, typlag, 0.d0, lisrel)
        endif
! ---    DEUXIEME GROUPE DE RELATIONS SI LE NOEUD COURANT
! ---    PORTE LES DDLS DE ROTATION
        if ((exisdg(zi(jprnm-1+ (in-1)*nbec+1),icmp4)) .and.&
            (exisdg( zi(jprnm-1+ (in-1)*nbec+1),icmp5)) .and.&
            (exisdg(zi(jprnm-1+ ( in-1)*nbec+1),icmp6))) then
! ---       QUATRIEME RELATION : DRX(M) - DRX(A)  = 0
            nbterm = 2
            zk8(jlisno+1-1) = zk8(ilisno+j-1)
            zk8(jlisno+2-1) = nomnoe
            zk8(jlisdl+1-1) = 'DRX'
            zk8(jlisdl+2-1) = 'DRX'
            zr(jliscr+1-1) = un
            zr(jliscr+2-1) = -un
            call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                        zr(jlisdi), nbterm, beta, betac, betaf,&
                        typcoe, typval, typlag, 0.d0, lisrel)
! ---       CINQUIEME RELATION : DRY(M) - DRY(A)  = 0
            zk8(jlisdl+1-1) = 'DRY'
            zk8(jlisdl+2-1) = 'DRY'
            call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                        zr(jlisdi), nbterm, beta, betac, betaf,&
                        typcoe, typval, typlag, 0.d0, lisrel)
! ---       SIXIEME RELATION : DRZ(M) - DRZ(A)  = 0
            zk8(jlisdl+1-1) = 'DRZ'
            zk8(jlisdl+2-1) = 'DRZ'
            call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                        zr(jlisdi), nbterm, beta, betac, betaf,&
                        typcoe, typval, typlag, 0.d0, lisrel)
        endif
40  end do
! --- DESTRUCTION DES OBJETS DE TRAVAIL
    call jedetr('&&DRZ13D.LISNO')
    call jedetr('&&DRZ13D.LISDDL')
    call jedetr('&&DRZ13D.COER')
    call jedetr('&&DRZ13D.COEC')
    call jedetr('&&DRZ13D.DIRECT')
    call jedetr('&&DRZ13D.DIME')
!
    call jedema()
end subroutine
