subroutine xprini(model, noma, cnxinv, grille, fispre,&
                  fiss, cnsln, cnslt, cnsgls, noesom,&
                  noresi, vcn, grlr, lcmin)
    implicit none
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/padist.h"
#include "asterfort/wkvect.h"
#include "asterfort/xprcfl.h"
#include "asterfort/xprcnu.h"
!
    character(len=8) :: model, noma, fispre, fiss
    character(len=19) :: cnsln, cnslt, cnsgls, noesom, noresi, cnxinv
    character(len=24) :: vcn, grlr
    logical :: grille
    real(kind=8) :: lcmin
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
! person_in_charge: patrick.massin at edf.fr
!     ------------------------------------------------------------------
!
!       XPRINI   : X-FEM PROPAGATION : INITIALISATION DES PARAMETRES DE
!       ------     -     --            ---         XPRREI, XPRREO ET
!                                                  XPRUPW
!
!    ENTREE
!        MODEL   : NOM DU CONCEPT MODELE
!        NOMA    : NOM DU CONCEPT MAILLAGE
!        CNXINV  : CONNECTIVITE INVERSEE DU MAILLAGE NOMA
!        GRILLE  : .TRUE. SI MODEL EST UNE GRILLE AUXILIAIRE
!                  .FALSE. SI MODEL N'EST PAS UNE GRILLE AUXILIAIRE
!        FISPRE  : NOM DE LA FISSURE PRECEDENTE
!        FISS    : NOM DE LA FISSURE CALCULEE
!        CNSLN   : CHAM_NO_S DES VALEURS DE LEVEL SET NORMALE
!        CNSLT   : CHAM_NO_S DES VALEURS DE LEVEL SET TANGENTE
!        CNSGLS  : CHAM_NO_S DES VALEURS DU GRADIENT DE LS
!
!    SORTIE
!        NOESOM  : VECTEUR LOGIQUE INDIQUANT SI LE NOEUD EST SOMMET
!        NORESI  : VECTEUR LOGIQUE INDIQUANT SI LE RESIDU DOIT ETRE
!                  ESTIME SUR LE NOEUD
!
!    EN PLUS, SI GRILLE=.FALSE. ET SI LA METHODE UPWIND EST UTILISEE,
!    ON A ON SORTIE LES OBJETS SUIVANTES:
!        VCN     : VOIR XPRCNU.F POUR LA DESCRIPTION DE CETTE OBJET.
!                  POUR LA METHODE SIMPLEXE, CET OBJET N'EST PAS UTILISE
!        GRLR    : VOIR XPRCNU.F POUR LA DESCRIPTION DE CETTE OBJET
!                  POUR LA METHODE SIMPLEXE, CET OBJET N'EST PAS UTILISE
!
!    SI GRILLE=.FALSE.
!        LCMIN   : LONGUEUR DE LA PLUS PETITE ARETE DE LA GRILLE
!
!     ------------------------------------------------------------------
!
!
    integer :: i, ino, ima, ifm, niv, jglsno, iret, iret2, nbno, jnosom, jnresi
    integer :: nbma, jcoor, jconx1, jconx2, jlnno, jltno, nbmaff, nnores, jmaiff
    integer :: nbnoma, inoa, inob, nunoa, nunob, ibid
    character(len=8) :: lpain(4), lpaout(2), method, nomno
    character(len=19) :: celmt, maiff
    character(len=24) :: ligrel, lchin(1), lchout(2)
    real(kind=8) :: p(3), ff(3), dist, lsna, lsnb, lsta, lstb, rayon
    logical :: coupln, couplt
    real(kind=8) :: damax
!
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
!  RECUPERATION DU MODELE ET DU MAILLAGE
    ligrel = model//'.MODELE'
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbno)
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
!   RECUPERATION DE LA METHODE DE REINITIALISATION A EMPLOYER
    call getvtx(' ', 'METHODE', scal=method, nbret=ibid)
!
    if ((method.eq.'UPWIND') .and. (.not.grille)) then
        call xprcnu(noma, cnxinv, 'V', vcn, grlr,&
                    lcmin)
    endif
!
    if ((method.ne.'UPWIND') .and. (.not.grille)) then
        call xprcfl(model, lcmin)
    else
        write(ifm,*)'   LONGUEUR DE LA PLUS PETITE ARETE DU MAILLAGE:'&
     &               //' ',lcmin
    endif
!
    if (method .eq. 'GEOMETRI') goto 9999
!
!  RECUPERATION DU RAYON DU TORE OU L'ON ESTIME LE RESIDU
    call getvr8(' ', 'RAYON', scal=rayon, nbret=iret)
!
!   RETRIEVE THE MAXIMUM ADVANCEMENT OF THE CRACK FRONT
    call getvr8(' ', 'DA_MAX', scal=damax, nbret=iret)
!
!   RECUPERATION DES VALEURS DES LS ET DU GRADIENT DE LS
    call jeveuo(cnsln//'.CNSV', 'E', jlnno)
    call jeveuo(cnslt//'.CNSV', 'E', jltno)
    call jeveuo(cnsgls//'.CNSV', 'E', jglsno)
!
!-----------------------------------------------------------------------
    if (method .eq. 'SIMPLEXE') then
!
        call jeexin(fispre//'.PRO.MES_EL', iret)
        call jeexin(fispre//'.PRO.NORMAL', iret2)
        if (iret .eq. 0 .or. iret2 .eq. 0) then
!------------------------------------------------------
!   CALCUL DE |T| ET DES DIRECTIONS NI SUR LES ELEMENTS
!------------------------------------------------------
            celmt = '&&XPRINI.CELMT'
            lpain(1)='PGEOMER'
            lchin(1)=noma//'.COORDO'
            lpaout(1)='PMEAST'
            lchout(1)=celmt
            lpaout(2)='PNIELNO'
            lchout(2)=fiss//'.PRO.NORMAL'
!
            call calcul('S', 'XFEM_SMPLX_INIT', ligrel, 1, lchin,&
                        lpain, 2, lchout, lpaout, 'V',&
                        'OUI')
!
            call celces(celmt, 'V', fiss//'.PRO.MES_EL')
            call jedetr(celmt)
!
        else
            call jedupo(fispre//'.PRO.MES_EL', 'G', fiss// '.PRO.MES_EL', .false.)
            call jedupo(fispre//'.PRO.NORMAL', 'G', fiss// '.PRO.NORMAL', .false.)
        endif
!
    endif
!-----------------------------------------------------------------------
!
!------------------------------------------------------------------
!     ON REPERE LES NOEUDS SOMMETS (DONT LE GRADIENT DE LS EST NUL)
!------------------------------------------------------------------
    call wkvect(noesom, 'V V L', nbno, jnosom)
    do 100 ino = 1, nbno
        zl(jnosom-1+ino) = .true.
        call jenuno(jexnum(noma //'.NOMNOE', ino), nomno)
        if (nomno(1:2) .eq. 'NS') zl(jnosom-1+ino) = .false.
!
!         NORMGR = SQRT( ZR(JGLSNO-1+NDIM*(INO-1)+1)**2 +
!     &                  ZR(JGLSNO-1+NDIM*(INO-1)+2)**2 +
!     &                  ZR(JGLSNO-1+NDIM*(INO-1)+3)**2 )
!
!  LES NOEUDS DONT LE GRADIENT DE LS EST NUL SONT DES NOEUDS MILIEUX
!         IF (NORMGR.LT.R8PREM()) ZL(JNOSOM-1+INO) = .FALSE.
!
100 end do
!
!-----------------------------------------------------------------
!     ON REPERE LES NOEUDS SUR LESQUELS LE RESIDU DOIT ETRE ESTIME
!-----------------------------------------------------------------
!  VECTEUR CONTENANT LES COORDONNEES DES MAILLES COUPEES (BARYCENTRES)
    maiff = '&&XPRINI.MAIFF'
    call wkvect(maiff, 'V V R', nbma*3, jmaiff)
!
    nbmaff = 0
    do 200 ima = 1, nbma
        coupln = .false.
        couplt = .false.
        nbnoma = zi(jconx2+ima) - zi(jconx2+ima-1)
!  ON PARCOURS LES ARETES DE L'ELEMENT
        do 210 inoa = 1, nbnoma-1
            nunoa = zi(jconx1-1+zi(jconx2+ima-1)+inoa-1)
!  ON ECARTE LES NOEUDS MILIEUX
            if (.not.zl(jnosom-1+nunoa)) goto 210
            lsna = zr(jlnno-1+nunoa)
            lsta = zr(jltno-1+nunoa)
!
            do 220 inob = inoa+1, nbnoma
                nunob = zi(jconx1-1+zi(jconx2+ima-1)+inob-1)
!  ON ECARTE LES NOEUDS MILIEUX
                if (.not.zl(jnosom-1+nunob)) goto 220
                lsnb = zr(jlnno-1+nunob)
                lstb = zr(jltno-1+nunob)
!
                if ((lsna*lsnb) .le. 0.d0) coupln=.true.
                if ((lsta*lstb) .le. 0.d0) couplt=.true.
!
220         continue
210     continue
!
!  SI LA MAILLE EST COUPEE PAR LES ISOZEROS DE LS ET LT
        if (coupln .and. couplt) then
            nbmaff = nbmaff + 1
!
!  ON REPERE LE BARYCENTRE DES NOEUDS DE LA MAILLE
            do 230 i = 1, 3
                zr(jmaiff-1+3*(nbmaff-1)+i) = 0.d0
230         continue
            do 240 inoa = 1, nbnoma
                nunoa = zi(jconx1-1+zi(jconx2+ima-1)+inoa-1)
                do 245 i = 1, 3
                    zr(jmaiff-1+3*(nbmaff-1)+i) = zr(&
                                                  jmaiff-1+3*( nbmaff-1)+i) + zr(jcoor-1+3*(nunoa&
                                                  &-1)+i&
                                                  ) / nbnoma
245             continue
240         continue
        endif
!
200 end do
!
    nnores=0
    call wkvect(noresi, 'V V L', nbno, jnresi)
    do 250 ino = 1, nbno
        zl(jnresi-1+ino) = .false.
!  ON ECARTE LES NOEUDS MILIEUX
        if (.not.zl(jnosom-1+ino)) goto 250
        do 260 i = 1, 3
            p(i) = zr(jcoor-1+3*(ino-1)+i)
260     continue
!
        do 270 ima = 1, nbmaff
            do 275 i = 1, 3
                ff(i) = zr(jmaiff-1+3*(ima-1)+i)
275         continue
            dist = padist(3,p,ff)
            if (dist .le. (rayon+damax)) then
!  LE NOEUD EST PROCHE D'UNE MAILLE DU FOND DE FISSURE
                zl(jnresi-1+ino) = .true.
                nnores = nnores+1
                goto 250
            endif
270     continue
250 end do
!      IF (NIV.GT.1)
    write(ifm,*)'   NOMBRE DE NOEUDS POUR L'''&
     &                         //'ESTIMATION DES RESIDUS :',nnores
!
    call jedetr(maiff)
!
9999 continue
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedema()
end subroutine
