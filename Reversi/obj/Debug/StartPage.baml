   M S B A M L   `   `   `  ����B  >Reversi, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null!clr-namespace:ReversiReversi  5          Reversi.StartPage   5      W SPresentationCore, Version=4.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35R NWindowsBase, Version=4.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35\ XPresentationFramework, Version=4.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35D 9http://schemas.microsoft.com/winfx/2006/xaml/presentation    5      R NSystem.Xaml, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089:x,http://schemas.microsoft.com/winfx/2006/xaml     5      Jmc;http://schemas.openxmlformats.org/markup-compatibility/2006     5      @d2http://schemas.microsoft.com/expression/blend/2008     5      'localclr-namespace:Reversi     5        N� Title$  	StartPage��5	         BackgroundLinearGradient N� 
Background# �   6    � 	Resources 5
      ��%    5       � Triggers 5      :� 5   
    :� RoutedEvent$ FrameworkElement.Loaded��6   .[�5      ��   EnterHeadingText#��[ 6   5      .��5      �  � RowDefinitions 5   
   �� 5      $��4* �6   �� 5      .+�5   
   �� -   5         ��Name$ reversiTextLabel��5      $
��Left=�6*   $��Center=�6E   $��50,0,0,0q�6`    J� RenderTransformOrigin$ 0.5,0.5&�6r   z�5      V� 5      .~�5      �� �� 5      �� 5      T� 5      5      .��5      ��  �� Orientation$ Vertical=�6   .+�5       �� $��72(�6   $
��Left=�6*   $��Segoe UI Light)�6E     PrimaryBrush#���  6a   �� 6�   $��96(�6�   $�Reversi��6�   6�   �� 5!      $��16(�6   $��9,0,0,0q�6*   �� 5"      $��Segoe UI Semibold)�6   $�Made by��69     	GrayBrush#���  6H    �� 5#      $��Segoe UI Semibold)�6   $�Oliver Zhang��69     
BlackBrush#���  6M   5$      5%      5&      A� 5'   
   $��1��6   #���  6#   	 A� Columns$	 3��6O   .+�5(      �� -   5        
 ��Name$

 Help��5(        WhiteButton#��[ 6#   $
��Help��6K   6Z   �� 5)      -   5        $
 
Highscores��5)      #��[ 6)   $��
Highscores��6Q   6f   �� 5*      -   5        $
 PlayGame��5*      #��[ 6'   $��	Play Game��6O   6c   5+      5,      5-      