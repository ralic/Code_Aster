subroutine te0003(option, nomte)
!-----------------------------------------------------------------------
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
! person_in_charge: olivier.boiteau at edf.fr
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  CALCUL DE L'ESTIMATEUR D'ERREUR EN RESIDU
!      SUR UN ELEMENT ISOPARAMETRIQUE 2D/3D, LUMPE OU NON, VIA L'OPTION
!     'ERTH_ELEM'
!
! IN OPTION : NOM DE L'OPTION
! IN NOMTE  : NOM DU TYPE D'ELEMENT
!   -------------------------------------------------------------------
!     SUBROUTINES APPELLEES:
!       MESSAGE:INFNIV,U2MESS,U2MESG.
!       JEVEUX:JEMARQ,JEDEMA,JEVETE,JEVEUO,JELIRA,JENUNO.
!       CHAMP LOCAUX:JEVECH,TECACH.
!       ENVIMA:R8PREM,R8MIEM.
!       ELEMENTS FINIS:DFDM2D,DFDM3D.
!       MATERIAUX/CHARGES:RCVALA,RCCOMA,FOINTE.
!       NON LINEAIRE: NTFCMA,RCFODE.
!       DEDIEES A TE0003:UTHK,UTJAC,UTINTC,UTIN3D,UTNORM,UTNO3D,
!                        UTVOIS,UTERFL,UTEREC,UTERSA,UTNBNV.
!     FONCTIONS INTRINSEQUES:
!       ABS,SQRT,SIGN.
!   -------------------------------------------------------------------
!     ASTER INFORMATIONS:
!       22/06/01 (OB): CREATION EN S'INSPIRANT DE TE0377.F (2D) ET DE
!                      TE0375.F (3D).
!       05/02/02 (OB): EXTENSION AUX EFS LUMPES.
!       10/06/02 (OB): CORRECTION BUG DES ELREFE.
!       22/08/02 (OB): RAJOUT TABNIV POUR NIVEAU DIFFERENCIE SI INFO=2
!               RAJOUT DE L'OBJET '&&RESTHE.JEVEUO' POUR AMELIORER LES
!               PERFORMANCES DE RESTHE/CALCUL/TE0003.
!       12/09/02 (OB): ELIMINATION ALARME DU A LA DIVISION PAR ZERO
!                POUR CONSTRUIRE ERTREL + AFFICHAGES SUPL. SI INFO=2.
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
! aslint: disable=W1501
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterc/r8miem.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elref1.h"
#include "asterfort/elref4.h"
#include "asterfort/elref7.h"
#include "asterfort/fointe.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jevech.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/lteatt.h"
#include "asterfort/ntfcma.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcfode.h"
#include "asterfort/rcvala.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/uterec.h"
#include "asterfort/uterfl.h"
#include "asterfort/utersa.h"
#include "asterfort/uthk.h"
#include "asterfort/utin3d.h"
#include "asterfort/utintc.h"
#include "asterfort/utjac.h"
#include "asterfort/utmess.h"
#include "asterfort/utnbnv.h"
#include "asterfort/utno3d.h"
#include "asterfort/utnorm.h"
#include "asterfort/utvois.h"
!
    character(len=16) :: option, nomte
!
!
! DECLARATION VARIABLES LOCALES
    integer :: ifm, niv
    integer :: tabniv(20)
    integer :: iadzi, iazk24
    integer :: ibid, itab(7), iret, iret2
    integer :: noe(9, 6, 4)
    integer :: igeom
    integer :: ierr, ivois
    integer :: imate
    integer :: isour, icharg
    integer :: iflum, iflup, itemm, itemp
    integer :: ndim
    integer :: nno, nnos, npg, ipoids, ivf, idfde, jgano
    integer :: ndimf
    integer :: nnof, nnosf, npgf, ipoidf, ivff, idfdxf, idfdyf, jganof
    integer :: nno2, nnos2, npg2, ipoid2, ivf2, idfdx2, idfdy2, jgano2
    integer :: i, j, k, icode, i1, ij, i2, mceld, mcelv, pceld, pcelv, iaux1
    integer :: iavaf, ncmpf, iavah, ncmph, jno, mno, imav, iaptma, ientf, ienth
    integer :: ientt, iavat, ncmpt, nbsv, nbnv, jad, jadv, igrel, iel, iavalp
    integer :: iavalm, iarepe, niveau, ifon(3), nbpar, ijeveo
    integer :: ipg
    integer :: nbf
    integer :: tymvol, ndegre, ifa, tyv
!
    real(kind=8) :: r8bid, r8bid3(4)
    real(kind=8) :: insold, inst, valthe, aux, rhocp(1), dfdx(27), dfdy(27), poids
    real(kind=8) :: r, valfp(9), valfm(9), r8cart, valsp, valsm, valunt, terbuf
    real(kind=8) :: tempm, tempp, fluxm(3), fluxp(3), fforme, deltat, prec, ovfl
    real(kind=8) :: der(6), flurp, flurm, unsurr, x3, y3, xn(9), yn(9), zn(9)
    real(kind=8) :: term22, jac(9), hk, hf, zrjno1, zrjno2, zrino1, zrino2
    real(kind=8) :: poinc1, poinc2, valhp(9), valhm(9), valtp(9), valtm(9)
    real(kind=8) :: r8car1, ertabs, ertrel, termno, termvo, termsa, termfl
    real(kind=8) :: termec, termv1, termv2, terms1, terms2, termf1, termf2
    real(kind=8) :: terme1, terme2, jacob, unsurd, rhocpm, rhocpp, dfdz(27), x
    real(kind=8) :: y, z
!
    integer :: icodre(2)
    character(len=2) :: formv
    character(len=4) :: evol
    character(len=8) :: typmav, elrefe
    character(len=8) :: elreff, elrefb
    character(len=8) :: ma, nompar(4), typmac, k8cart, k8car1
    character(len=16) :: phenom
    character(len=19) :: cartef, carteh, cartet, cartes, nomgdf, nomgdh, nomgdt
    character(len=19) :: nomgds, ligrel, chflum, chflup
    character(len=24) :: valk(2)
    logical :: levol, ltheta, laxi, lmaj, lnonli, l2d, llumpe
!
! --- INITIALISATION DU TABLEAU DES NUMEROS DE NOEUDS FACE PAR FACE ----
!     REMARQUE : NE PAS SUPPRIMER CE COMMENTAIRE DETAILLE CAR IL EST
!                COMMUN A TOUS LES PROGRAMMES UTILISANT NOE
!
!     NOE (IN,IFA,TYMVOL) : IN     : NUMERO DU NOEUD DANS LA FACE
!                           IFA    : NUMERO DE LA FACE
!                           TYMVOL : TYPE DE LA MAILLE VOLUMIQUE
!                                    1 : HEXAEDRE A 8,20 ET 27 NOEUDS
!                                    2 : PENTAEDRE A 6 ET 15 NOEUDS
!                                    3 : TETRAEDRE A 4 ET 10 NOEUDS
!                                    4 : PYRAMIDE A 5 ET 13 NOEUDS
!  ==> POUR LE IN-EME NOEUD DE LA IFA-EME FACE D'UNE MAILLE DE TYPE
!      TYMVOL, NOE (IN,IFA,TYMVOL) EST SON NUMERO LOCAL DANS LA
!      DESCRIPTION DE LA MAILLE VOLUMIQUE.
!   . ON CHOISIT UNE ORIENTATION SORTANTE POUR DECRIRE UNE FACE.
!   . L'ORDRE DES FACES N'A PAS D'IMPORTANCE EN SOI MAIS IL FAUT
!     ETRE COHERENT :
!      - MEME DESCRIPTION PAR LES SOMMETS POUR LES VOISINAGES (RESVOI)
!      - CONVENTION POUR LE PENTAEDRE ET DE LA PYRAMIDE POUR LESQUELS
!        ON COMMENCE PAR LES TRIANGLES.
!
!      ON RAPPELLE QU'EN FORTRAN L'ORDRE DE RANGEMENT EST LE SUIVANT :
!   (1,1,1) (2,1,1) (3,1,1) ... (9,1,1) (1,2,1) (2,2,1) ... (9,2,1)
!   (1,3,1)  ...    (8,6,4) (9,6,4)
!    ON COMMENCE AINSI PAR LES 9 NOEUDS DE LA 1ERE FACE DE L'HEXAEDRE,
!    PUIS LES 9 NOEUDS DE LA 2EME FACE DE L'HEXAEDRE,
!    ETC JUSQU'AUX 9 NOEUDS DE LA 6EME FACE DE L'HEXAEDRE.
!    ENSUITE ON A LES 6 NOEUDS DE LA 1ERE FACE DU PENTAEDRE, ETC
!
! CONVENTIONS ASTER POUR UN HEXAEDRE :
!  (N1,N2,N3,N4) EST ENTRANT
!  (N5,N6,N7,N8) EST SORTANT (DONC DANS LE MEME SENS QUE (N1,N2,N3,N4))
!  N9 EST AU MILIEU DE (N1,N2)
! N10 EST AU MILIEU DE (N2,N3)
! N11 EST AU MILIEU DE (N3,N4)
! N12 EST AU MILIEU DE (N4,N1)
! N13 EST AU MILIEU DE (N1,N5)
! N14 EST AU MILIEU DE (N2,N6)
! N15 EST AU MILIEU DE (N3,N7)
! N16 EST AU MILIEU DE (N4,N8)
! N17 EST AU MILIEU DE (N5,N6)
! N18 EST AU MILIEU DE (N6,N7)
! N19 EST AU MILIEU DE (N7,N8)
! N20 EST AU MILIEU DE (N8,N5)
!
!             N1                  N2
!             ---------N9----------
!            /                   /.
!        N12/                  N10.
!          /                   /  .
!         /   N13             /   .
!      N4 ---------N11--------N3  .N14
!         .                  .    .
!         .                  .    .
!         .    .N5     N17   .    .N6
!      N16.                  N15 /
!         . N20              .  /N18
!         .                  . /
!         .                  ./
!         ---------N19--------
!       N8                    N7
!
!
! CONVENTIONS ASTER POUR UN PENTAEDRE :
!  (N1,N2,N3) EST ENTRANT
!  (N4,N5,N6) EST SORTANT (DONC DANS LE MEME SENS QUE (N1,N2,N3))
!  N7 EST AU MILIEU DE (N1,N2)
!  N8 EST AU MILIEU DE (N2,N3)
!  N9 EST AU MILIEU DE (N3,N1)
! N10 EST AU MILIEU DE (N1,N4)
! N11 EST AU MILIEU DE (N2,N5)
! N12 EST AU MILIEU DE (N3,N6)
! N13 EST AU MILIEU DE (N4,N5)
! N14 EST AU MILIEU DE (N5,N6)
! N15 EST AU MILIEU DE (N6,N4)
!
!          N2                     N11                  N5
!           X------------------------------------------X
!          .                                          .
!         .  .                                       .  .
!        .                                          .
!     N8.     .                                 N14.     .
!      .                                          .
!     .        .                                 .        .
!    .          N7                              .           N13
! N3.           .         N12                 N6.           .
!  X------------------------------------------X
!     .          .                               .          .
!          .                                      N15 .
!              .  .                                       .  .
!        N9        X------------------------------------------X
!                 N1                     N10                  N4
!
! CONVENTIONS ASTER POUR UN TETRAEDRE :
!  (N1,N2,N3) EST ENTRANT
!  N5 EST AU MILIEU DE (N1,N2)
!  N6 EST AU MILIEU DE (N2,N3)
!  N7 EST AU MILIEU DE (N3,N1)
!  N8 EST AU MILIEU DE (N1,N4)
!  N9 EST AU MILIEU DE (N2,N4)
! N10 EST AU MILIEU DE (N3,N4)
!
!                     N1
!                     *
!                    .  ..
!                   .     . .
!                  .        .  *N8
!                 .           .   .
!                .              *    .  N4
!             N7*               N5.    *
!              .                  . .   .
!             .              .        .  .
!            .          *N10             . *N9
!           .      .                      ..
!          .  .                             .
!         *................*.................*
!       N3                 N6                N2
!
!
! CONVENTIONS ASTER POUR UNE PYRAMIDE :
!  (N1,N2,N3,N4) EST ENTRANT
!  N5 EST LE SOMMET OPPOSE AU QUADRANGLE
!  N6 EST AU MILIEU DE (N1,N2)
!  N7 EST AU MILIEU DE (N2,N3)
!  N8 EST AU MILIEU DE (N3,N4)
!  N9 EST AU MILIEU DE (N4,N1)
! N10 EST AU MILIEU DE (N1,N5)
! N11 EST AU MILIEU DE (N2,N5)
! N12 EST AU MILIEU DE (N3,N5)
! N13 EST AU MILIEU DE (N4,N5)
!                            N5
!                            X
!                         . . . .
!                       .  .   .   .
!                     .   .   N13     .
!                   .    .       .       .
!                 .     .        X .         .
!             N10.      .     .   N4    .        .N12
!             .       .  .                 .       .
!           .        .                         .      .
!         .      .  .N11                           .     .
!       .    .     .                            N8     .    .
!     .  .  N9    .                                        .   .
! N1 .           .                                             .  .
!  X .         .                                                  .  .
!         .    .                                                      .
!       N6    X--------------------------------------------------------X
!           N2                           N7                          N3
!
    data noe/1,4,3,2,12,11,10, 9,21, 1,2,6,5, 9,14,17,13,22,&
     &         2,3,7,6,10,15,18,14,23, 3,4,8,7,11,16,19,15,24,&
     &         4,1,5,8,12,13,20,16,25, 5,6,7,8,17,18,19,20,26,&
     &         1,3,2,9,8,7, 3*0,       4,5,6,13,14,15, 3*0,&
     &         1,2,5,4, 7,11,13,10, 0, 2,3,6,5,8,12,14,11,0,&
     &         1,4,6,3,10,15,12, 9, 0,  9*0,&
     &         1,3,2,7,6, 5, 3*0,      2,3,4,6,10,9, 3*0,&
     &         3,1,4,7,8,10, 3*0,      1,2,4,5, 9,8, 3*0,&
     &         9*0,                    9*0,&
     &         1,2,5,6,11,10, 3*0,     2,3,5,7,12,11, 3*0,&
     &         3,4,5,8,13,12, 3*0,     4,1,5,9,10,13, 3*0,&
     &         1,4,3,2,9,8,7,6, 0,     9*0 /
!--------------------------------------------------------------------
! INITIALISATIONS/RECUPERATION DE LA GEOMETRIE ET DES CHAMPS LOCAUX
!--------------------------------------------------------------------
    1000 format(a,' :',(6(1x,1pe17.10)))
    1001 format(a,' :',9i10)
! 2000 FORMAT(A,10I8)
! ----------------------------------------------------------------------
! 1 -------------- GESTION DES DONNEES ---------------------------------
! ----------------------------------------------------------------------
    call jemarq()
!
    call infniv(ifm, niv)
!
    call tecael(iadzi, iazk24)
    valk(1)=zk24(iazk24-1+3)
    valk(2)=option
!
    call elref1(elrefe)
!
! NIVEAU D'AFFICHAGE DIFFERENCIE PAR ROUTINE (0: RIEN, 2 AFFICHAGE)
    do 10 ibid = 1, 20
        tabniv(ibid) = 0
10  end do
    if (niv .eq. 2) then
! INFO GENERALES SUR L'ELEMENT K
        tabniv(1) = 2
        tabniv(2) = 0
! POUR UTHK: INFO SUR LE DIAMETRE DE L'ELEMENT K
        tabniv(3) = 2
! AFFICHAGE DU PHENOMENE DE LA MAILLE ET DU RHOCP
        tabniv(4) = 0
! AFFICHAGE DU JACOBIEN
        tabniv(5) = 2
! AFFICHAGE DES DETAILS DE CONSTRUCTION DU TERME SOURCE
        tabniv(6) = 0
! AFFICHAGE DU TOTAL DU TERME SOURCE
        tabniv(7) = 2
! POUR UTNORM: INFO SUR HF, NORMAL, CONNECTIQUE DES FACES
        tabniv(8) = 0
! POUR UTINTC/3D: INTERPOLATION FLUX + INFO CARTE
        tabniv(9) = 0
! POUR UTERFL: CALCUL TERME ERREUR FLUX
        tabniv(10) = 0
! AFFICHAGE DU TOTAL DU TERME DE FLUX
        tabniv(11) = 2
! POUR UTINC/3D: INTERPOLATION ECHANGE + INFO CARTE
        tabniv(12) = 0
! POUR UTERFL: CALCUL TERME ERREUR ECHANGE
        tabniv(13) = 0
! AFFICHAGE DU TOTAL DU TERME D'ECHANGE
        tabniv(14) = 2
! POUR UTERSA: CALCUL TERME ERREUR SAUT
        tabniv(15) = 0
! AFFICHAGE DU TOTAL DU TERME DE SAUT
        tabniv(16) = 2
    endif
!
    if (tabniv(1) .eq. 2) then
        write(ifm,*) ' '
        write(ifm,*) '================================================='
        write(ifm,*) ' '
        write(ifm,*) 'MAILLE NUMERO', zi(iadzi),', DE TYPE ', elrefe
    endif
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
!
    call jevech('PERREUR', 'E', ierr)
    do 1 ibid = 1, 14
        zr(ierr-1+ibid)=0.d0
 1  end do
!
! INITIALISATION DIMENSION DE L'ESPACE DE TRAVAIL/LLUMPE OU PAS
    llumpe = lteatt(' ','LUMPE','OUI')
    l2d = (ndim.eq.2)
    if (llumpe) then
        if (( elrefe(1:3).eq.'H20' ) .or. ( elrefe(1:3).eq.'H27' ) .or.&
            ( elrefe(1:3).eq.'P15' ) .or. ( elrefe(1:3).eq.'T10' ) .or.&
            ( elrefe(1:3).eq.'P13' )) then
            ASSERT(.false.)
        endif
    endif
!
! INIT. GENERALES (PARAM NUMERIQUE, INTERPOLATION, FLAG SI AXI...)
    prec = r8prem()
    ovfl = r8miem()
    nbpar = ndim + 1
    nompar(1) = 'X'
    nompar(2) = 'Y'
    nompar(nbpar) = 'INST'
    laxi = lteatt(' ','AXIS','OUI')
    if (.not.l2d) then
        nompar(3) = 'Z'
    endif
!
! RECUPERATION DES ADRESSES DES CHAMPS LOCAUX ASSOCIES AUX PARAMETRES:
! FLUX TEMP - ET +(IFLUM ET IFLUP),SOURCE (ISOUR),
! MATERIAU (IMATE),CHARGE (ICHARG),
! FLAG EVOL='EVOL' OU '',THETA (VALTHE),INSTANT - ET + (INSOLD/INST)
    call jevech('PCHARG', 'L', icharg)
    evol = zk24(icharg+15)(1:4)
    if (evol .eq. 'EVOL') then
        call jevech('PTEMP_M', 'L', itemm)
        call jevech('PFLUX_M', 'L', iflum)
        read (zk24(icharg+13),'(F19.8)') insold
        read (zk24(icharg+12),'(F19.8)') valthe
        valunt = 1.d0 - valthe
        levol = .true.
    else
        valthe = 1.d0
        valunt = 0.d0
        levol = .false.
    endif
!
    read (zk24(icharg+14),'(F19.8)') inst
! FIN DE LECTURE DE LA CARTE ETENDUE: LECTURE VECT '&&RESTHE.JEVEUO'
    read (zk24(icharg+16),'(I24)') ijeveo
    niveau = zi(ijeveo)
    ifm = zi(ijeveo+1)
    niv = zi(ijeveo+2)
    iarepe = zi(ijeveo+3)
    mceld = zi(ijeveo+4)
    mcelv = zi(ijeveo+5)
    pceld = zi(ijeveo+6)
    pcelv = zi(ijeveo+7)
    iavaf = zi(ijeveo+8)
    ncmpf = zi(ijeveo+9)
    iavah = zi(ijeveo+10)
    ncmph = zi(ijeveo+11)
    iavat = zi(ijeveo+12)
    ncmpt = zi(ijeveo+13)
!
    call jevech('PTEMP_P', 'L', itemp)
    call jevech('PFLUX_P', 'L', iflup)
    call jevech('PMATERC', 'L', imate)
!
!
! RECUPERATION DES CARTES FLUX, CHARGEMENT ET DE LEUR TYPE
    ma = zk24(icharg)(1:8)
    chflum = zk24(icharg+2)(1:19)
    chflup = zk24(icharg+3)(1:19)
    cartef = zk24(icharg+4)(1:19)
    nomgdf = zk24(icharg+5)(1:19)
    carteh = zk24(icharg+6)(1:19)
    nomgdh = zk24(icharg+7)(1:19)
    cartet = zk24(icharg+8)(1:19)
    nomgdt = zk24(icharg+9)(1:19)
    cartes = zk24(icharg+10)(1:19)
    nomgds = zk24(icharg+11)(1:19)
!
! LA SOURCE PEUT ETRE REELLE OU FONCTION (OU ABSENTE) :
    call tecach('ONN', 'PSOURCR', 'L', iret, iad=isour)
    if (iret .ne. 0) then
        call tecach('ONN', 'PSOURCF', 'L', iret2, iad=isour)
        if (iret2 .eq. 0) ASSERT(nomgds.eq.'SOUR_F')
    endif
!
! FLAG POUR EFFECTUER LES CALCULS IMPLIQUANT (1-THETA)
    ltheta = (valunt.gt.prec)
!
! IMPRESSIONS NIVEAU 2 POUR DIAGNOSTIC
    if (tabniv(2) .eq. 2) then
        write (ifm,*) 'NOMTE : ',nomte,', L2D : ',l2d
        if (l2d) write (ifm,*) 'LAXI =',laxi
        write (ifm,*) 'VALTHE =',valthe,', VALUNT =',valunt
        write (ifm,*) 'LEVOL =',levol,', LTHETA =',ltheta
        ligrel = zk24(icharg+1)(1:19)
        write (ifm,*) 'MA = ',ma,', LIGREL = ',ligrel
        write (ifm,*) 'CHFLUM = ',chflum,', CHFLUP = ',chflup
        write (ifm,*) 'CARTEF = ',cartef,', NOMGDF = ',nomgdf
        write (ifm,*) 'CARTEH = ',carteh,', NOMGDH = ',nomgdh
        write (ifm,*) 'CARTET = ',cartet,', NOMGDT = ',nomgdt
        write (ifm,*) 'CARTES = ',cartes,', NOMGDS = ',nomgds
    endif
!
! ----------------------------------------------------------------------
! ---------------- CALCUL DU PREMIER TERME DE L'ERREUR -----------------
! ----------------------------------------------------------------------
!
! ----- CALCUL DU DIAMETRE HK DE LA MAILLE ----
    call uthk(nomte, zr(igeom), hk, ndim, itab,&
              ibid, ibid, ibid, tabniv(3), ifm)
!
!------------------------------------------------------------------
! CALCUL DU TERME VOLUMIQUE
!------------------------------------------------------------------
!
! RECHERCHE DE LA VALEUR DE RHO*CP EN LINEAIRE ET EN NON-LINEAIRE
    call rccoma(zi(imate), 'THER', 1, phenom, icodre(1))
    ASSERT(icodre(1).eq.0)
    if ((phenom.eq.'THER') .or. (phenom.eq.'THER_ORTH')) then
        lnonli = .false.
        call rcvala(zi(imate), ' ', phenom, 1, 'INST',&
                    [inst], 1, 'RHO_CP', rhocp(1), icodre,&
                    1)
        if (icodre(1) .ne. 0) then
            call utmess('F', 'ELEMENTS2_62')
        endif
    else if (phenom.eq.'THER_NL') then
        lnonli = .true.
        call ntfcma(zi(imate), ifon)
        call utmess('A', 'ELEMENTS4_91')
    else
        call utmess('F', 'ELEMENTS2_63')
    endif
    if (tabniv(4) .eq. 2) then
        write (ifm,*) 'PHENOM =',phenom
        if (.not.lnonli) write (ifm,1000) 'RHOCP',rhocp(1)
    endif
!
!---------------------------------
! ----- CALCUL DU TERME D'ERREUR AVEC INTEGRATION DE GAUSS -------------
!---------------------------------
    termvo = 0.d0
    termv1 = 0.d0
    aux = 0.d0
    lmaj = .false.
    do 12 , ipg = 1,npg
!
    terbuf = 0.d0
    k = (ipg-1)*nno
! FONCTIONS DE FORME ET LEURS DERIVEES
    if (l2d) then
        call dfdm2d(nno, ipg, ipoids, idfde, zr(igeom),&
                    poids, dfdx, dfdy)
    else
        call dfdm3d(nno, ipg, ipoids, idfde, zr(igeom),&
                    poids, dfdx, dfdy, dfdz)
    endif
!
! CALCUL L'ORIENTATION DE LA MAILLE
    call utjac(l2d, zr(igeom), ipg, idfde, tabniv(5),&
               ifm, nno, jacob)
!
!---------------------------------
! CALCUL DE LA PARTIE SOURCE (THETA * S+ + (1-THETA) * S-)
!---------------------------------
!---------------------------------
! CAS SOURCE VARIABLE
!---------------------------------
    if (nomgds .eq. 'SOUR_F') then
!
! SOURCE FONCTION (R,Z,TEMPS) (VALSP/M S+/- AU POINT DE GAUSS)
        x = 0.d0
        y = 0.d0
        z = 0.d0
        valsp = 0.d0
        valsm = 0.d0
        do 20 i = 1, nno
            i1 = i - 1
            i2 = igeom + ndim*i1
            fforme = zr(ivf+k+i1)
            x = x + zr(i2)*fforme
            y = y + zr(i2+1)*fforme
            if (.not.l2d) z = z + zr(i2+2)*fforme
20      continue
        r8bid3(1) = x
        r8bid3(2) = y
        if (.not.l2d) r8bid3(3) = z
        r8bid3(nbpar) = inst
        call fointe('FM', zk8(isour), nbpar, nompar, r8bid3,&
                    valsp, icode)
        if (ltheta) then
            r8bid3(nbpar) = insold
            call fointe('FM', zk8(isour), nbpar, nompar, r8bid3,&
                        valsm, icode)
            terbuf = valthe*valsp + valunt*valsm
        else
            terbuf = valthe*valsp
        endif
        if (tabniv(6) .eq. 2) then
            write (ifm,1000) 'X / Y / Z / INST ',x,y,z,inst
            write (ifm,1000) 'TERMVO SOUR_F',valsp,valsm
        endif
!
!---------------------------------
! CAS SOURCE CONSTANTE
!---------------------------------
    else if (nomgds.eq.'SOUR_R') then
        terbuf = zr(isour+ipg-1)
        if (tabniv(6) .eq. 2) write (ifm,1000) 'TERMVO SOUR_R', terbuf
    endif
!
    if (terbuf .gt. prec) lmaj = .true.
! CALCUL PRELIMINAIRE POUR TERMNO (TERME SOURCE DE NORMALISATION)
    aux = terbuf
!
!---------------------------------
! CALCUL DE LA PARTIE DIFFERENCE FINIE (RHOCP*DELTA TEMP/DELTA T)
!---------------------------------
!
    if (levol) then
!
! TEMPM/P T-/+ AU POINT DE GAUSS
        tempm = 0.d0
        tempp = 0.d0
        do 30 i = 1, nno
            i1 = i - 1
            fforme = zr(ivf+k+i1)
            tempm = tempm + zr(itemm+i1)*fforme
            tempp = tempp + zr(itemp+i1)*fforme
30      continue
        deltat = inst - insold
        ASSERT(deltat.gt.ovfl)
        unsurd = 1.d0/deltat
        if (lnonli) then
! CAS NON LINEAIRE
! INTERPOLATION DERIVEE CHAMP D'ENTHALPIE IFON(1) A T- ET T+
            call rcfode(ifon(1), tempm, r8bid, rhocpm)
            call rcfode(ifon(1), tempp, r8bid, rhocpp)
            terbuf = terbuf - (rhocpp-rhocpm)*unsurd
            if (tabniv(6) .eq. 2) write (ifm,1000) 'RHOCPP/M', rhocpp,rhocpm
        else
! CAS LINEAIRE
            terbuf = terbuf - (tempp-tempm)*rhocp(1)*unsurd
        endif
! IMPRESSIONS NIVEAU 2 POUR DIAGNOSTIC...
        if (tabniv(6) .eq. 2) write (ifm, 1000) 'TERMVO TEMPP/M/DELTAT', tempp, tempm, deltat
    endif
!
!---------------------------------
! CALCUL DE LA PARTIE DIVERGENCE (DIV(THETA*Q+ + (1-THETA)*Q-))
!---------------------------------
!
    do 40 i = 1, ndim
        fluxm(i) = 0.d0
        fluxp(i) = 0.d0
40  continue
!
! TRAITEMENT PARTICULIER DU A L'AXI (PART I)
    if (laxi) then
        r = 0.d0
        flurm = 0.d0
        flurp = 0.d0
    endif
!
    do 60 i = 1, nno
        i1 = i - 1
        i2 = i1*ndim
        der(1) = dfdx(i)
        der(2) = dfdy(i)
! TRAITEMENT PARTICULIER DU A L'AXI (PART II)
        if (laxi) then
            der(4) = zr(ivf+k+i1)
            r = r + zr(igeom+i2)*der(4)
            flurp = flurp + zr(iflup+i2)*der(4)
            if (ltheta) flurm = flurm + zr(iflum+i2)*der(4)
        endif
        if (.not.l2d) der(3) = dfdz(i)
        do 50 j = 1, ndim
            ij = i2 + j - 1
            fluxp(j) = fluxp(j) + zr(iflup+ij)*der(j)
            if (ltheta) fluxm(j) = fluxm(j) + zr(iflum+ij)*der(j)
50      continue
60  continue
!
! TRAITEMENT PARTICULIER DU A L'AXI (PART III)
    if (laxi) then
        ASSERT(abs(r).gt.ovfl)
        unsurr = 1.d0/r
        poids = poids*r
    endif
    if (l2d) then
        terbuf = terbuf - valthe* (fluxp(1)+fluxp(2))
        if (tabniv(6) .eq. 2) write (ifm, 1000) 'TERMVO FLUXP', fluxp(1), fluxp(2)
        if (laxi) then
            terbuf = terbuf - valthe*unsurr*flurp
            if (tabniv(6) .eq. 2) write (ifm, 1000) 'TERMVO FLURP/R', flurp*unsurr, r
        endif
        if (ltheta) then
            terbuf = terbuf - valunt* (fluxm(1)+fluxm(2))
            if (tabniv(6) .eq. 2) write (ifm, 1000) 'TERMVO FLUXM', fluxm(1), fluxm(2)
            if (laxi) then
                terbuf = terbuf - valunt*unsurr*flurm
                if (tabniv(6) .eq. 2) write (ifm, 1000) 'TERMVO FLURM', flurm*unsurr
            endif
        endif
    else
        terbuf = terbuf - valthe* (fluxp(1)+fluxp(2)+fluxp(3))
        if (tabniv(6) .eq. 2) write (ifm, 1000) 'TERMVO FLUXP', fluxp(1), fluxp(2), fluxp(3)
        if (ltheta) then
            terbuf = terbuf - valunt* (fluxm(1)+fluxm(2)+fluxm(3))
            if (tabniv(6) .eq. 2) write (ifm, 1000) 'TERMVO FLUXM', fluxm(1), fluxm(2), fluxm(3)
        endif
    endif
    termvo = termvo + terbuf*terbuf*poids
    termv1 = termv1 + aux*aux*poids
!
!---------------------------------
! FIN BOUCLE SUR LES POINTS DE GAUSS --------------------------------
!---------------------------------
    12 end do
!
! CALCUL FINAL DU TERME VOLUMIQUE ET DU TERME SOURCE DE NORMALISATION
    termvo = hk*sqrt(termvo)
    termv1 = hk*sqrt(termv1)
    if (tabniv(7) .eq. 2) write (ifm,1000) '---> TERMVO/TERMV1', termvo,termv1
!
!
! ----------------------------------------------------------------------
! ------------ FIN DU CALCUL DU PREMIER TERME DE L'ERREUR --------------
! ----------------------------------------------------------------------
!
! ----------------------------------------------------------------------
! --------- CALCUL DES DEUXIEME ET TROISIEME TERMES DE L'ERREUR --------
! ----------------------------------------------------------------------
!
    call jevech('PVOISIN', 'L', ivois)
!
    if (l2d) then
! TYPE DE LA MAILLE COURANTE
        ibid = zi(ivois+7)
        call jenuno(jexnum('&CATA.TM.NOMTM', ibid), typmac)
! CALCUL DES CARACTERISTIQUES DE SES ARETES (OU DE SES FACES) ET
! RECUPERATION DE SES DERIVEES DE FONCTIONS DE FORME (EN 3D ONLY).
        call utvois(typmac, lmaj, nbf, nnosf, poinc1,&
                    poinc2, elreff, ndegre)
    else
!
! --------- INFORMATIONS SUR LA MAILLE COURANTE : --------------------
!       TYMVOL : TYPE DE LA MAILLE VOLUMIQUE
!       NDEGRE : DEGRE DE L'ELEMENT
!       NBF    : NOMBRE DE FACES DE LA MAILLE VOLUMIQUE
!       ELREFF : DENOMINATION DE LA MAILLE FACE DE ELREFE - FAMILLE 1
!       ELREFB : DENOMINATION DE LA MAILLE FACE DE ELREFE - FAMILLE 2
!      --- REMARQUE : ON IMPOSE UNE FAMILLE DE POINTS DE GAUSS
!
        call elref7(elrefe, tymvol, ndegre, nbf, elreff,&
                    elrefb)
!
!GN      WRITE(6,*) 'TYPE MAILLE VOLUMIQUE COURANTE :',TYMVOL
! --- CARACTERISTIQUES DES FACES DE BORD DE LA FAMILLE 1 ---------------
!GN      WRITE(IFM,*) 'ELREFF : ',ELREFF
        call elref4(elreff, 'MASS', ndimf, nnof, nnosf,&
                    npgf, ipoidf, ivff, idfdxf, jganof)
        idfdyf = idfdxf + 1
!GN      WRITE(IFM,2000) 'NDIMF,NNOF',NDIMF,NNOF
!GN      WRITE(IFM,2000) 'NNOSF,NPGF',NNOSF,NPGF
!GN      WRITE(IFM,1000) 'IPOIDF', (ZR(IPOIDF+IFA),IFA=0,NPGF-1)
!
! --- COMPLEMENT EVENTUEL POUR LES MAILLES QUI ONT 2 TYPES DE ---
! --- MAILLES DE BORD (PENTAEDRE, PYRAMIDE) ---
!
        if (elrefb(1:1) .ne. ' ') then
            call elref4(elrefb, 'NOEU', ndimf, nno2, nnos2,&
                        npg2, ipoid2, ivf2, idfdx2, jgano2)
            idfdy2 = idfdx2 + 1
!GN       WRITE(IFM,2000) 'NDIMF,NNO2',NDIMF,NNO2
!GN       WRITE(IFM,2000) 'NNOS2,NPG2',NNOS2,NPG2
!GN       WRITE(IFM,1000) 'IPOID2', (ZR(IPOID2+IFA),IFA=0,NPG2-1)
        endif
!
    endif
!
    if (tabniv(1) .eq. 2) then
        write (ifm,*) 'DIAMETRE ',hk
        if (l2d) then
            write (ifm,*) ' ARETES DE TYPE ',elreff
        else
            write (ifm,*) ' FACES DE TYPE   ',elreff,elrefb
        endif
        write (ifm,*) '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
    endif
!---------------------------------
! BOUCLE SUR LES ARETES OU LES FACES DE LA MAILLE VOLUMIQUE -------
!---------------------------------
    termsa = 0.d0
    terms1 = 0.d0
    termfl = 0.d0
    termf1 = 0.d0
    termec = 0.d0
    terme1 = 0.d0
    do 300 , ifa=1,nbf
!
! ------TEST DU TYPE DE VOISIN -----------------------------------------
!
    tyv=zi(ivois+7+ifa)
!
    if (tyv .ne. 0) then
!
! ------- RECUPERATION DU TYPE DE LA MAILLE VOISINE
!
        call jenuno(jexnum('&CATA.TM.NOMTM', tyv), typmav)
        if (tabniv(1) .eq. 2) then
            write(ifm,*) 'IFA =', ifa,' ==> TYPMAV = ', typmav
        endif
!---------------------------------
! CALCULS PRELIMINAIRES
!---------------------------------
!
! CALCUL HF + POINTS INTEGRATION + NORMALE/JACOBIEN
        if (l2d) then
! CAS 2D
            call utnorm(igeom, nnosf, nbf, ifa, poinc1,&
                        poinc2, jno, mno, zrino2, zrino1,&
                        zrjno2, zrjno1, x3, y3, hf,&
                        xn, yn, jac, laxi, jacob,&
                        ifm, tabniv(8))
        else
!
! CAS 3D
!
! --- QUAND ON ARRIVE AUX FACES QUAD DES PENTAEDRES OU DES PYRAMIDES ---
! --- IL FAUT REMPLACER LES CARACTERISTIQUES DE LA FAMILLE 1         ---
! --- PAR CELLES DE LA FAMILLE 2                                     ---
!
            if (( tymvol.eq.2 .and. ifa.ge.3 ) .or. ( tymvol.eq.4 .and. ifa.ge.5 )) then
!
                nnof = nno2
                npgf = npg2
                nnosf = nnos2
                ipoidf = ipoid2
                idfdxf = idfdx2
                idfdyf = idfdy2
!
            endif
!
            ibid = ifa
            call utno3d(ifm, tabniv(8), nnosf, ibid, tymvol,&
                        igeom, xn, yn, zn, jac,&
                        idfdxf, idfdyf, hf, zr(ipoidf), npgf,&
                        noe)
!
        endif
!
! TEST DU TYPE DE LA MAILLE VOISINE PARTAGEANT L'ARETE/LA FACE IFA
        formv = typmav(1:2)
!
! NUMERO DE CETTE MAILLE VOISINE
        imav = zi(ivois+ifa)
!
        if (tabniv(1) .eq. 2) then
            write(ifm,1003) ifa, imav, typmav
            1003 format (i2,'-EME FACE DE NUMERO',i10,' ==> TYPMAV = ', a)
            call jeveuo(jexnum(ma//'.CONNEX', imav), 'L', iaux1)
            write (ifm,1001) '<<< DE PREMIERS SOMMETS ', (zi(&
                iaux1+ibid),ibid=0,nnosf-1)
        endif
!
! ----------------------------------------------------------------------
! --------------- CALCUL DU TROISIEME TERME DE L'ERREUR ----------------
! --------------- LE BORD VOISIN EST UNE MAILLE DE PEAU ----------------
! ----------------------------------------------------------------------
!
        if ((l2d.and. ((formv.eq.'SE').or. (formv.eq.'SL'))) .or.&
            (&
            .not.l2d .and.&
            ((formv.eq.'QU') .or. (formv.eq.'TR') .or. (formv.eq.'QL') .or. (formv.eq.'TL')&
            )&
            )) then
!
!---------------------------------
! CALCUL DE LA PARTIE FLUX
!           (THETA*(F+ + NU.Q+)+(1-THETA)*(F- + NU.Q-))
!---------------------------------
!
! POINTEUR CARTE FLUX IMPOSE
            if (cartef .ne. ' ') then
                call jeexin(cartef//'.PTMA', iret)
                if (iret .eq. 0) then
! CARTE CONSTANTE
                    ientf = 1
                else
! LA CARTE A ETE ETENDUE
                    call jeveuo(cartef//'.PTMA', 'L', iaptma)
                    ientf = zi(iaptma-1+imav)
                endif
            endif
!
!---------------------------------
! CAS FLUX VARIABLE
!---------------------------------
!
            lmaj = .false.
            if (nomgdf(1:6) .eq. 'FLUN_F') then
!
! FLUX IMPOSE FONCTION (VALFP/M(I)=Q+/- AU POINT IFA/JNO/MNO)
                k8cart = zk8(iavaf+ (ientf-1)*ncmpf)
                if (tabniv(9) .eq. 2) write (ifm,*) 'TERMFL FLUN_F ',k8cart
                if (k8cart .ne. '&FOZERO') then
!
                    lmaj = .true.
! INTERPOLATION F AUX INSTANTS +/-
                    if (l2d) then
                        call utintc(zrino2, zrino1, zrjno2, zrjno1, x3,&
                                    y3, inst, insold, k8cart, ltheta,&
                                    nnosf, valfp, valfm, tabniv(9), ifm,&
                                    1)
                    else
                        call utin3d(igeom, nnosf, ifa, tymvol, inst,&
                                    insold, k8cart, ltheta, tabniv(9), ifm,&
                                    1, valfp, valfm, noe)
                    endif
!
! CALCUL DU TERME D'ERREUR ET DE NORMALISATION ASSOCIE
                    call uterfl(ndim, iflup, iflum, ifa, mno,&
                                jno, nnosf, jac, term22, aux,&
                                ltheta, valthe, valunt, tabniv(10), ifm,&
                                xn, yn, zn, valfp, valfm,&
                                tymvol, noe)
                endif
!
!---------------------------------
! CAS FLUX CONSTANT
!---------------------------------
            else if (nomgdf(1:6).eq.'FLUN_R') then
!
! FLUX IMPOSE CONSTANT (VALFP(I)=Q AU POINT IFA/JNO/MNO VALFM(I)=0)
                r8cart = zr(iavaf+ (ientf-1)*ncmpf)
                if (tabniv(9) .eq. 2) write (ifm,*) 'TERMFL FLUN_R', r8cart
                if (abs(r8cart) .gt. prec) then
!
                    lmaj = .true.
! CALCUL DU TERME D'ERREUR ET DE NORMALISATION ASSOCIE
                    do 100 i = 1, nnosf
                        valfp(i) = r8cart
100                  continue
                    if (ltheta) then
                        do 110 i = 1, nnosf
                            valfm(i) = r8cart
110                      continue
                    endif
                    call uterfl(ndim, iflup, iflum, ifa, mno,&
                                jno, nnosf, jac, term22, aux,&
                                ltheta, valthe, valunt, tabniv(10), ifm,&
                                xn, yn, zn, valfp, valfm,&
                                tymvol, noe)
                endif
            endif
!
! MISE A JOUR DU TERME DE FLUX
            if (lmaj) then
                term22 = sqrt(hf*term22)
                aux = sqrt(hf*aux)
                termfl = termfl + term22
                termf1 = termf1 + aux
                if (tabniv(11) .eq. 2) then
                    write (ifm,1000) '---> TERMFL/TERMF1 ',term22,&
                        aux
                    write (ifm,*)
                endif
                lmaj = .false.
            endif
!
!---------------------------------
! CALCUL DE LA PARTIE ECHANGE AVEC L'EXTERIEUR
! (THETA*(H+*(TEXT+ - T+)+NU.Q+)+(1-THETA)*(H-*(TEXT- - T-)+NU.Q-)
!---------------------------------
!
            if (carteh .ne. ' ') then
                call jeexin(carteh//'.PTMA', iret)
                if (iret .eq. 0) then
                    ienth = 1
                else
                    call jeveuo(carteh//'.PTMA', 'L', iaptma)
                    ienth = zi(iaptma-1+imav)
                endif
                call jeexin(cartet//'.PTMA', iret)
                if (iret .eq. 0) then
                    ientt = 1
                else
                    call jeveuo(cartet//'.PTMA', 'L', iaptma)
                    ientt = zi(iaptma-1+imav)
                endif
            endif
!
! COMPTE-TENU DU PERIMETRE DE AFFE_CHAR_THER ON A SIMULTANEMENT
! (COEH_F,TEMP_F) OU  (COEH_R,TEMP_R)
!---------------------------------
! CAS ECHANGE VARIABLE
!---------------------------------
!
            lmaj = .false.
            if (nomgdh(1:6) .eq. 'COEH_F') then
!
! COEF_H IMPOSE FONCTION (VALHP/M(I)=Q+/- AU POINT IFA/JNO/MNO)
                k8cart = zk8(iavah+ (ienth-1)*ncmph)
! TEMPERATURE IMPOSEE FONCTION (VALTP/M(I)=TEXT+/-  IFA/JNO/MNO)
                k8car1 = zk8(iavat+ (ientt-1)*ncmpt)
                if (tabniv(12) .eq. 2) write (ifm, *) 'TERMEC COEH_F ', k8cart, ' / ', k8car1
!
                if ((k8cart.ne.'&FOZERO') .and. (k8car1.ne.'&FOZERO')) then
!
                    lmaj = .true.
                    if (l2d) then
! INTERPOLATION H AUX INSTANTS +/-
                        call utintc(zrino2, zrino1, zrjno2, zrjno1, x3,&
                                    y3, inst, insold, k8cart, ltheta,&
                                    nnosf, valhp, valhm, tabniv(12), ifm,&
                                    2)
! INTERPOLATION TEXT AUX INSTANTS +/-
                        call utintc(zrino2, zrino1, zrjno2, zrjno1, x3,&
                                    y3, inst, insold, k8car1, ltheta,&
                                    nnosf, valtp, valtm, tabniv(12), ifm,&
                                    3)
                    else
                        call utin3d(igeom, nnosf, ifa, tymvol, inst,&
                                    insold, k8cart, ltheta, tabniv(12), ifm,&
                                    2, valhp, valhm, noe)
                        call utin3d(igeom, nnosf, ifa, tymvol, inst,&
                                    insold, k8car1, ltheta, tabniv(12), ifm,&
                                    3, valtp, valtm, noe)
                    endif
!
! CALCUL DU TERME D'ERREUR ET DE NORMALISATION ASSOCIE
                    call uterec(ndim, iflup, iflum, ifa, mno,&
                                jno, nnosf, jac, term22, aux,&
                                ltheta, valthe, valunt, tabniv(13), ifm,&
                                xn, yn, zn, valhp, valhm,&
                                valtp, valtm, tymvol, itemp, itemm,&
                                noe)
!
                endif
!
!---------------------------------
! CAS ECHANGE CONSTANT
!---------------------------------
            else if (nomgdh(1:6).eq.'COEH_R') then
!
! COEF_H IMPOSE CONSTANT (VALHP(I)=Q AU POINT IFA/JNO/MNO VALHM(I)=0)
                r8cart = zr(iavah+ (ienth-1)*ncmph)
! TEMPERATURE IMPOSEE CONSTANT (VALTP(I)=TEXT+  IFA/JNO/MNO VALTM(I)=0)
                r8car1 = zr(iavat+ (ientt-1)*ncmpt)
                if (tabniv(12) .eq. 2) write (ifm, 1000) 'TERMEC COEH_R', r8cart, r8car1
!
                if ((abs(r8cart).gt.prec) .or. (abs(r8car1) .gt.prec)) then
!
                    lmaj = .true.
! CALCUL DU TERME D'ERREUR ET DE NORMALISATION ASSOCIE
                    do 120 i = 1, nnosf
                        valhp(i) = r8cart
                        valtp(i) = r8car1
120                  continue
                    if (ltheta) then
                        do 130 i = 1, nnosf
                            valhm(i) = r8cart
                            valtm(i) = r8car1
130                      continue
                    endif
                    call uterec(ndim, iflup, iflum, ifa, mno,&
                                jno, nnosf, jac, term22, aux,&
                                ltheta, valthe, valunt, tabniv(13), ifm,&
                                xn, yn, zn, valhp, valhm,&
                                valtp, valtm, tymvol, itemp, itemm,&
                                noe)
                endif
            endif
!
! CALCUL FINAL DU TERME D'ECHANGE
            if (lmaj) then
                term22 = sqrt(hf*term22)
                aux = sqrt(hf*aux)
                termec = termec + term22
                terme1 = terme1 + aux
                if (tabniv(14) .eq. 2) then
                    write (ifm,1000) '---> TERMEC/TERME1',term22,&
                        aux
                    write (ifm,*)
                endif
                lmaj = .false.
            endif
!
! ----------------------------------------------------------------------
! --------------- CALCUL DU DEUXIEME TERME DE L'ERREUR -----------------
! --------------- LE BORD VOISIN EST UN VOLUME -------------------------
! ----------------------------------------------------------------------
!
!---------------------------------
! CALCUL DE LA PARTIE SAUT DU FLUX CALCULE
!---------------------------------
!
            else if ((l2d.and. ((formv.eq.'TR').or. (formv.eq.'QU')&
            .or. (formv.eq.'TL').or. (formv.eq.'QL'))) .or. (&
            .not.l2d.and. ((formv.eq.'HE').or. (formv.eq.'PE').or.&
            (formv.eq.'TE')))) then
!
! CALCUL DU TYPE DE MAILLE VOISINE ET DE SON NBRE DE SOMMETS
            call utnbnv(typmav, nbsv, nbnv)
!
! NUMERO DU LIGREL DE LA MAILLE VOISINE DE NUMERO GLOBAL IMAV
            igrel = zi(iarepe+2* (imav-1))
! INDICE DE LA MAILLE VOISINE DANS LE IGREL
            iel = zi(iarepe+2* (imav-1)+1)
            if (tabniv(15) .eq. 2) write (ifm,*) 'IGREL/IEL',igrel, iel
!
! INDICE DE DEPART DANS LA SD .CELV DU FLUX + ET -. ADDRESSE
! DANS ZR DE LA CMP 1 DU PT 1 DE LA MAILLE 1 DU GREL IEL
            iavalp = pcelv - 1 + zi(pceld-1+zi(pceld-1+4+igrel)+8)
            if (ltheta) iavalm = mcelv - 1 + zi(mceld-1+zi( mceld-1+4+igrel)+8)
!
! CALCUL DU TERME D'ERREUR ET DE NORMALISATION ASSOCIE
            call utersa(ndim, iflup, iflum, ifa, mno,&
                        jno, ivois, ma, iel, nbnv,&
                        nbsv, iavalp, iavalm, nnosf, jac,&
                        ltheta, valthe, valunt, tabniv(15), ifm,&
                        tymvol, xn, yn, zn, term22,&
                        aux, jad, jadv, noe)
!
! CALCUL FINAL DU TERME DE SAUT
            term22 = 0.5d0*sqrt(hf*term22)
            aux = 0.5d0*sqrt(hf*aux)
            termsa = termsa + term22
            terms1 = terms1 + aux
            if (tabniv(16) .eq. 2) then
                write (ifm,1000) '---> TERMSA/TERMS1',term22,aux
                write (ifm,*)
            endif
!
! ----------------------------------------------------------------------
! --------------- CURIEUX ----------------------------------------------
! ----------------------------------------------------------------------
!
        else
!
            valk(1)=typmav(1:4)
            call utmess('F', 'INDICATEUR_10', sk=valk(1))
!---------------------------------
! FIN IF FORMV                    --------------------------------
!---------------------------------
        endif
!---------------------------------
! FIN IF (TYV.NE.0) THEN          --------------------------------
!---------------------------------
    endif
!---------------------------------
! FIN DE BOUCLE SUR LES ARETES    --------------------------------
!---------------------------------
    300 end do
!
!---------------------------------
! MISE EN FORME DES DIFFERENTS TERMES DE ERRETEMP
!---------------------------------
!
! ERREUR TOTALE
    ertabs = termvo + termsa + termfl + termec
    termno = termv1 + terms1 + termf1 + terme1
    ertrel = 0.d0
    if (termno .gt. ovfl) ertrel = (100.d0*ertabs)/termno
!
! ERREURS PARTIELLES
    if (niveau .eq. 2) then
        termv2 = 0.d0
        if (termv1 .gt. ovfl) termv2 = 100.d0* (termvo/termv1)
        terms2 = 0.d0
        if (terms1 .gt. ovfl) terms2 = 100.d0* (termsa/terms1)
        termf2 = 0.d0
        if (termf1 .gt. ovfl) termf2 = 100.d0* (termfl/termf1)
        terme2 = 0.d0
        if (terme1 .gt. ovfl) terme2 = 100.d0* (termec/terme1)
    endif
!
! STOCKAGE
    zr(ierr) = ertabs
    zr(ierr+1) = ertrel
    zr(ierr+2) = termno
    if (niveau .eq. 2) then
        zr(ierr+3) = termvo
        zr(ierr+4) = termv2
        zr(ierr+5) = termv1
        zr(ierr+6) = termsa
        zr(ierr+7) = terms2
        zr(ierr+8) = terms1
        zr(ierr+9) = termfl
        zr(ierr+10) = termf2
        zr(ierr+11) = termf1
        zr(ierr+12) = termec
        zr(ierr+13) = terme2
        zr(ierr+14) = terme1
    endif
!
    if (tabniv(1) .eq. 2) then
        write (ifm,*)
        write (ifm,*) '*********************************************'
        write (ifm,*) '     TOTAL SUR LA MAILLE ',zi(ivois)
        write (ifm,*)
        write (ifm,*) 'ERREUR             ABSOLUE   /  RELATIVE '//&
     &    '/ NORMALISATION'
        write (ifm,400) ' TOTAL           ',ertabs,ertrel,'%',termno
        write (ifm,400) ' TERME VOLUMIQUE ',termvo,termv2,'%',termv1
        write (ifm,400) ' TERME SAUT      ',termsa,terms2,'%',terms1
        write (ifm,400) ' TERME FLUX      ',termfl,termf2,'%',termf1
        write (ifm,400) ' TERME ECHANGE   ',termec,terme2,'%',terme1
        write (ifm,*) '*********************************************'
        write (ifm,*)
    endif
!
    call jedema()
    400 format (a17,d12.4,1x,d12.4,a2,1x,d12.4)
end subroutine
