subroutine nmcrga(sderro)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    character(len=24) :: sderro
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (SD GESTION ALGO)
!
! CREATION DE LA SD
!
! ----------------------------------------------------------------------
!
! NB: LA SD S'APPELLE SDERRO
!
! IN  SDERRO : SD ERREUR
!
! ----------------------------------------------------------------------
!
    integer :: zeven
    parameter   (zeven = 33)
    character(len=16) :: neven(zeven)
    character(len=8) :: ncret(zeven)
    integer :: vcret(zeven)
    character(len=16) :: teven(zeven)
    character(len=24) :: feven(zeven), meven(zeven)
!
    integer :: ifm, niv
    integer :: ieven
    character(len=24) :: errecn, errecv, erreni, erreno, erraac, errfct, errmsg
    integer :: jeecon, jeecov, jeeniv, jeenom, jeeact, jeefct, jeemsg
    character(len=24) :: errinf, errcvg, errevt
    integer :: jeinfo, jeconv, jeeevt
!
! --- NOM DES EVENEMENTS
!
    data neven   /'ERRE_INTE','INTE_NPHY','DIVE_DEBO',&
     &              'INTE_BORN',&
     &              'ERRE_PILO','CONV_PILO','ERRE_FACS',&
     &              'ERRE_FACT','ERRE_CTD1','ERRE_CTD2',&
     &              'ERRE_TIMN','ERRE_TIMP','ERRE_EXCP',&
     &              'ITER_MAXI',&
     &              'DIVE_RESI','RESI_MAXR','RESI_MAXN',&
     &              'DIVE_PFIX','CRIT_STAB','DIVE_FIXG',&
     &              'DIVE_FIXF','DIVE_FIXC','ERRE_CTCG',&
     &              'ERRE_CTCF','ERRE_CTCC','DIVE_FROT',&
     &              'DIVE_GEOM','DIVE_RELA','DIVE_MAXI',&
     &              'DIVE_REFE','DIVE_COMP','DIVE_CTCC',&
     &              'SOLV_ITMX'/
!
! --- NOM DU CODE RETOUR ATTACHE A L'EVENEMENT
!
    data ncret   /'LDC','LDC','LDC',&
     &              'LDC',&
     &              'PIL','PIL','FAC',&
     &              'FAC','CTC','CTC',&
     &              'XXX','XXX','XXX',&
     &              'XXX',&
     &              'XXX','XXX','XXX',&
     &              'XXX','XXX','XXX',&
     &              'XXX','XXX','XXX',&
     &              'XXX','XXX','XXX',&
     &              'XXX','XXX','XXX',&
     &              'XXX','XXX','XXX',&
     &              'RES'/
!
! --- VALEUR DU CODE RETOUR CORRESPONDANT A CHAQUE EVENEMENT
!
    data vcret   / 1 , 2, 3,&
     &               4 ,&
     &               1 , 2, 1,&
     &               2 , 1, 2,&
     &               99,99,99,&
     &               99,&
     &               99,99,99,&
     &               99,99,99,&
     &               99,99,99,&
     &               99,99,99,&
     &               99,99,99,&
     &               99,99,99,&
     &               1 /
!
! --- TYPE ET NIVEAU DE DECLENCHEMENT POSSIBLES DE L'EVENEMENT
! TROIS TYPES
! EVEN  : EVENEMENT A CARACTERE PUREMENT INFORMATIF
!          -> PEUT ETRE TRAITE SI UTILISATEUR LE DEMANDE DANS
!             DEFI_LIST_INST
! ERRI_ : EVENEMENT A TRAITER IMMEDIATEMENT SI ON VEUT CONTINUER
! ERRC_ : EVENEMENT A TRAITER A CONVERGENCE
! CONV_ : EVENEMENT A TRAITER POUR DETERMINER LA CONVERGENCE
!
    data teven   /'ERRI_NEWT','ERRC_NEWT','CONV_NEWT',&
     &              'EVEN'     ,&
     &              'ERRI_NEWT','CONV_CALC','ERRI_NEWT',&
     &              'ERRI_NEWT','ERRI_NEWT','ERRI_NEWT',&
     &              'ERRI_CALC','ERRI_CALC','ERRI_CALC',&
     &              'ERRI_NEWT',&
     &              'EVEN'     ,'EVEN'     ,'EVEN'     ,&
     &              'CONV_NEWT','EVEN'     ,'CONV_FIXE',&
     &              'CONV_FIXE','CONV_FIXE','ERRI_FIXE',&
     &              'ERRI_FIXE','ERRI_FIXE','CONV_RESI',&
     &              'CONV_NEWT','CONV_RESI','CONV_RESI',&
     &              'CONV_RESI','CONV_RESI','CONV_NEWT',&
     &              'ERRI_NEWT'/
!
! --- FONCTIONNALITE ACTIVE SI NECESSAIRE POUR CONVERGENCE
!
    data feven   /' ',' '       ,' ',&
     &              ' ',&
     &              ' ','PILOTAGE',' ',&
     &              ' ',' '       ,' ',&
     &              ' ',' '       ,' ',&
     &              ' ',&
     &              ' ',' '       ,' ',&
     &              ' ',' '       ,' ',&
     &              ' ',' '       ,' ',&
     &              ' ',' '       ,' ',&
     &              ' ',' '       ,' ',&
     &              ' ',' '       ,' ',&
     &              'LDLT_SP'/
!
! --- CODE DU MESSAGE A AFFICHER
!
    data meven /&
     &        'MECANONLINE10_1' ,'MECANONLINE10_24',' ',&
     &        'MECANONLINE10_25',&
     &        'MECANONLINE10_2' ,' '               ,'MECANONLINE10_6' ,&
     &        'MECANONLINE10_6' ,'MECANONLINE10_4' ,'MECANONLINE10_4' ,&
     &        'MECANONLINE10_7' ,'MECANONLINE10_5' ,'MECANONLINE10_8' ,&
     &        'MECANONLINE10_3' ,&
     &        ' '               ,' '               ,' '               ,&
     &        ' '               ,'MECANONLINE10_20',' '               ,&
     &        ' '               ,' '               ,'MECANONLINE10_9' ,&
     &        'MECANONLINE10_10','MECANONLINE10_11',' '               ,&
     &        ' '               ,' '               ,' '               ,&
     &        ' '               ,' '               ,' '               ,&
     &        'MECANONLINE10_12'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... LECTURE GESTION ALGORITHME'
    endif
!
! --- GENERAL
!
    errinf = sderro(1:19)//'.INFO'
    call wkvect(errinf, 'V V I', 2, jeinfo)
    zi(jeinfo-1+1) = zeven
!
! --- OBJETS
!
    erreno = sderro(1:19)//'.ENOM'
    errecv = sderro(1:19)//'.ECOV'
    errecn = sderro(1:19)//'.ECON'
    erreni = sderro(1:19)//'.ENIV'
    errfct = sderro(1:19)//'.EFCT'
    erraac = sderro(1:19)//'.EACT'
    errcvg = sderro(1:19)//'.CONV'
    errevt = sderro(1:19)//'.EEVT'
    errmsg = sderro(1:19)//'.EMSG'
    call wkvect(erreno, 'V V K16', zeven, jeenom)
    call wkvect(errecv, 'V V I', zeven, jeecov)
    call wkvect(errecn, 'V V K8', zeven, jeecon)
    call wkvect(erreni, 'V V K16', zeven, jeeniv)
    call wkvect(errfct, 'V V K24', zeven, jeefct)
    call wkvect(erraac, 'V V I', zeven, jeeact)
    call wkvect(errcvg, 'V V I', 5, jeconv)
    call wkvect(errevt, 'V V K16', 2, jeeevt)
    call wkvect(errmsg, 'V V K24', zeven, jeemsg)
!
    do 10 ieven = 1, zeven
        zk16(jeenom-1+ieven) = neven(ieven)
        zk8 (jeecon-1+ieven) = ncret(ieven)
        zi (jeecov-1+ieven) = vcret(ieven)
        zk16(jeeniv-1+ieven) = teven(ieven)
        zk24(jeefct-1+ieven) = feven(ieven)
        zk24(jeemsg-1+ieven) = meven(ieven)
10  end do
!
    call jedema()
end subroutine
